import Crawler from 'crawler';
import fs from 'fs';
import robotsParser from 'robots-parser';
import fetch from 'node-fetch';

function getPlainLink(link) {
    const url = new URL(link);
    return url.origin + url.pathname;
}

function uniqContainer() {
    const uniqLinks = new Set();

    return {
        // IDNES is using query params mainly just for metrics -> we will ignore it for uniq links
        // ex. link: https://www.idnes.cz/kultura/film-televize/cesky-lev-arved-banger-irglova-jiri-havelka.A230304_091838_filmvideo_spm?zdroj=vybava_idnes
        add: function (link, onAdd) {
            const plainLink = getPlainLink(link);
            if (!uniqLinks.has(plainLink)) {
                uniqLinks.add(plainLink);
                onAdd(link);
            }
        },
    };
}

const HOMEPAGE = 'https://www.idnes.cz';
const USER_AGENT = 'CustomCrawler/0.0.1';
const uniqLinks = uniqContainer();
const articles = {};
const LIMIT = 1000;
/**
 * @type ReturnType<typeof robotsParser>
 */
let robots;
/**
 * @type Crawler
 */
let c;

// Add link only if same origin, allowed by robots.txt and comply with nofollow
function isLinkAllowed(a) {
    const link = a.attr('href');
    if (!link) return false;

    if (!link.startsWith(HOMEPAGE) || a.attr('rel') === 'nofollow') return false;

    return robots.isAllowed(link, USER_AGENT);
}

function callback(error, res, done) {
    if (error) return console.log(error);

    const link = getPlainLink(res.request.uri.href);
    console.log('processing', link);

    if (res.$('div.art-full p').length) {
        // Get active menu items -> corresponde to topic and subtopic of article
        const activeMenuItems = res
            .$('.portal .act > a')
            .map(function () {
                return res.$(this).text();
            })
            .get();
        const container = res.$('div.art-full');
        const authorsContainer = container.find('div.authors span[itemprop=author]');

        const article = {
            title: res.$('[itemprop="name headline"]').text(),
            topic: activeMenuItems[0] || '',
            subtopic: activeMenuItems[1] || '',
            opener: container.find('div.opener').text().trim(),
            paragraphs: container
                .find('div.text p')
                .map(function () {
                    return res.$(this).text().trim();
                })
                .get(),
            authors: authorsContainer
                .map(function () {
                    return {
                        name: res.$(this).find('[itemprop=name]').text(),
                        link: res.$(this).find('a').attr('href'),
                    };
                })
                .get(),
            actions: res
                .$('div.art-social a')
                .map(function () {
                    return {
                        title: res.$(this).attr('title'),
                        link: res.$(this).attr('href'),
                    };
                })
                .get(),
            datePublished: container.find('[itemprop=datePublished]').attr('content'),
            dateModified: container.find('[itemprop=dateModified]').attr('content'),
            premium: container.find('#paywall').length > 0 ? true : false,
            link,
        };

        // just in case if comments were loaded earlier
        if (articles[link]) articles[link] = { ...articles[link], ...article };
        else articles[link] = article;

        const discussAction = article.actions.find((a) => a.title === 'Diskuse');

        // Crawler discussion directly without inserting into queue
        if (discussAction)
            uniqLinks.add(discussAction.link, () => {
                c.direct({
                    uri: discussAction.link,
                    callback,
                });
            });

        if (Object.keys(articles).length >= LIMIT) {
            fs.writeFileSync('articles.json', JSON.stringify(Object.values(articles), null, 4));
            process.exit();
        }
    } else if (link.endsWith('/diskuse') && res.$('.disc-list .contribution .user-text').length) {
        const comments = res
            .$('.disc-list .contribution .user-text')
            // Just first 5 comments
            .slice(0, 5)
            .map(function () {
                return {
                    // Removed numbers which for some reason are inserted between letters
                    author: res.$(this).parent().find('.name a').text().trim().replace(/[0-9]/g, ''),
                    comment: res.$(this).text().trim(),
                };
            })
            .get();

        const articleLink = getPlainLink(res.$('.content .moot-art a').attr('href'));

        if (!articles[articleLink]) articles[articleLink] = { comments };
        else articles[articleLink].comments = comments;

        // exit prematurely -> do not crawle links from discuss page
        if (done) return done();
    }

    // Process links pointing to other articles
    const links = res
        .$('a.art-link')
        .filter(function () {
            return isLinkAllowed(res.$(this));
        })
        .map(function () {
            return res.$(this).attr('href');
        })
        .get()
        .filter(Boolean);

    links.forEach((link) => {
        uniqLinks.add(link, () => c.queue(link));
    });

    if (done) done();
}

async function main() {
    const robotsLink = `${HOMEPAGE}/robots.txt`;
    const res = await fetch(robotsLink);
    if (res.ok) {
        robots = robotsParser(robotsLink, await res.text());
    } else {
        // Fallback in case of missing robots.txt
        robots = robotsParser(robotsLink, ['User-agent: *', 'Allow: *']);
    }

    c = new Crawler({
        userAgent: USER_AGENT,
        // get crawl delay or fallback to 200ms
        rateLimit: robots.getCrawlDelay(USER_AGENT) ? robots.getCrawlDelay(USER_AGENT) * 1000 : 200,
        // This will be called for each crawled page
        callback,
    });

    c.queue(HOMEPAGE);
}

main();
