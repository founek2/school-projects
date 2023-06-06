# Web crawler definiton

Crawler Czech news website [Idnes](https://indes.cz) for articles and their comments. Features:

-   check for site map for more entries
-   follow robots.txt rules - with exception of retrieving comments
-   scrape artciles - parsing webpage will be website specific via css selectors
-   set custom User-Agent header
-   access every page max once
-   output will be parsed structures for articles in json format - `articles.json`
-   NodeJS will be used for crawling and parsing due to Author's language preference and async features
    -   [crawling](https://www.npmjs.com/package/crawler)
    -   [html parser](https://www.npmjs.com/package/cheerio) - jQuery implementation for backend
    -   [robots.txt parser](https://www.npmjs.com/package/robots-parser)
-   Ability to set how many articles crawler should retrieve
-   Select only links which will mostly likely point to other articles
-   Start crawling from homepage

## Example article

Following object is example of crawled article with descriptions for keys.

```js
{
    // Title of article
    title: "Další zlevňování elektřiny, Centropol srazí cenu všem stávajícím zákazníkům",
    // Topic retrieve from active menu items
    topic: "Ekonomika",
    subtopic: "Domácí",
    // intro article description
    opener: "Dodavatel Centropol snižuje cenu elektřiny u produktu Bez závazků na dobu neurčitou. Zlevnění je automatické, stávající zákazník pro to nemusí nic dělat. Oproti cenovému stropu, který na konci minulého roku stanovila vláda, domácnost ušetří 260 korun za megawatthodinu elektřiny.",
    // article itself
    paragraphs: [
        "Silová elektřina u produktu Bez závazku na dobu neurčitou bude od 17. dubna stát 4 740 korun za megawatthodinu (MWh) bez DPH. U produktu Bez závazku se stropem bude tato částka účinná již od 15. března. Zákazník, který elektřinou svítí, vaří a používá ji na běžné domácnosti, ušetří oproti stropu ročně zhruba tisíc korun.",
        "Pro zákazníky se staršími smlouvami na dobu neurčitou Centropol snižuje průměrnou cenu silové elektřiny na 4 950 korun za MWh bez DPH, přičemž jim zároveň nabídne přesmluvnění na produkt s lepší cenou.",
        "Dodavatelé energií v posledních týdnech a měsících uvádějí na trh nové a levnější ceníky každou chvíli. Vládní limit, nad který ceny nemohou jít, činí 5000 korun za MWh bez DPH, takže slevy se zatím pohybují ve stokorunách. Ještě v roce 2020 nabízeli dodavatelé dodávku elektřiny za méně než 2000 korun za megawatthodinu.",
        "Na energetické burze PXE se nyní velkoobchodní cena elektřiny pohybuje okolo 3300 korun za MWh, přičemž jde o dodávku v kalendářním roce 2024. Na nadcházející měsíc duben se elektřina obchoduje za 2500 korun. Dodavatelé nicméně elektřinu musí nakupovat pro své zákazníky průběžně, takže ji poptávali i v období minulého roku, kdy stála více než 10 tisíc korun.",
        "Základní lhůta pro podání daňového přiznání za zdaňovací období 2022 připadá letos na pondělí 3. dubna. Interaktivní daňové formuláře umí vypočítat daně za vás."
    ],
    // Authors of article
    authors: [
        {
            name: "Sára Mazúchová",
            link: "https://www.idnes.cz/novinari/sara-mazuchova.N4319"
        }
    ],
    // Actions available for article
    actions: [
        {
            title: "Sdílet na Facebooku",
            link: "http://www.facebook.com/share.php?u=https%3a%2f%2fwww.idnes.cz%2fekonomika%2fdomaci%2fcentropol-elektrina-dodavatel-cenik-pod-stropem.A230309_112810_ekonomika_maz%3futm_source%3dfacebook%26utm_medium%3dsharecd%26utm_campaign%3ddesktop"
        },
        {
            title: "Sdílet na Twitteru",
            link: "https://twitter.com/intent/tweet?text=Dal%c5%a1%c3%ad%20zlev%c5%88ov%c3%a1n%c3%ad%20elekt%c5%99iny,%20Centropol%20sraz%c3%ad%20cenu%20v%c5%a1em%20st%c3%a1vaj%c3%adc%c3%adm%20z%c3%a1kazn%c3%adk%c5%afm&related=iDNEScz&via=iDNEScz&url=https%3a%2f%2fwww.idnes.cz%2fekonomika%2fdomaci%2fcentropol-elektrina-dodavatel-cenik-pod-stropem.A230309_112810_ekonomika_maz%3futm_source%3dtwitter%26utm_medium%3dsharecd%26utm_campaign%3ddesktop"
        },
        {
            title: "Tisknout článek",
            link: "https://www.idnes.cz/ekonomika/domaci/centropol-elektrina-dodavatel-cenik-pod-stropem.A230309_112810_ekonomika_maz/tisk"
        },
        {
            title: "Diskuse",
            link: "https://www.idnes.cz/ekonomika/domaci/centropol-elektrina-dodavatel-cenik-pod-stropem.A230309_112810_ekonomika_maz/diskuse"
        }
    ],
    // Article metadata
    datePublished: "2023-03-09T11:54CET",
    dateModified: "2023-03-09T11:54CET",
    // Detection wheter article requires premium subscription -> if yes then part of article is hidden
    premium: false,
    // Link to article
    link: "https://www.idnes.cz/ekonomika/domaci/centropol-elektrina-dodavatel-cenik-pod-stropem.A230309_112810_ekonomika_maz",
    // First 5 comments from discussion
    comments: [
        {
            author: "Bruno Chojka",
            comment: "Reálná cena je 2 koruny za kilowatt, na tuhle cenu nikdo nemuze skocit"
        },
        {
            author: "Jan Malý",
            comment: "ČEZ loni prodal 90% plánované výroby r2023 bilaterálně za průměr 2700 Kč za mwh.....tak asi tak k těm cenám"
        },
        {
            author: "Josef Horálek",
            comment: "Jsem na spotu a moje průměná cena za silovku v Únoru 2023 bez DPH byla 2.90 Kč/Kwh."
        }
    ]
}
```

> Files [articles100.json](./results//articles100.json) and [articles1000.json](./results//articles1000.json) contains crawled data

## Performance tests

Rate limit set to 200ms:

-   100 articles -> 30 seconds
-   1000 articles -> 5 min

## Improvements

Used crawler package support proxies. With use of this feature it would allow to make more parallel requests without increasing change of getting banned. Also because of async nature of NodeJS is scales very eassilly until one thread for JS processing becames an issue - after that it would require change of architecture to multiple workers with shared queue.

Currently all articles has to fit into memory. This can be eliminated easilly for ex. by saving last 1000 articles to disk when 2000 in memory.

Could be extended to crawle only articles about specific topic (point of interest).

# Issues

-   I tried to use [sitemap parser](https://www.npmjs.com/package/sitemapper), but IDNES has it very big and it took just too much time and resources so I decided not to use it
