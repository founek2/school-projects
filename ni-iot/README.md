# NI-IOT

## Zadání

Cílem této práce bude vytvořit zařízení ovládání ohřevu a monitorování teploty vody v bojleru. Tento nápad vznikl z důvodu potřeby efektivnějšího využití elektrikté energie v průběhu dne v závislosti na výrobě fotovoltaické elektrárny. Tato elektrárna je již zaintegrovaná do mojí [IoT Platformi](https://iotdomu.cz) (vzniká v mém volném čase již 4 roky, [git](github.com/founek2/IOT-Platforma)) a vytvářené zařízení do ní bude také integrováno pomocí připravené Arduino knihovny https://github.com/founek2/IoT-Platform-library. Jako HW použiji zařízení [Sonoff TH Origin 16A Switch Module](https://templates.blakadder.com/sonoff_THR316.html) - nechci HW stavět z jednotlivých modulů, protože taková zařízení již mám, ale je s nimi spousta práce a nejsou úplně spolehlivá. Toto řešení má výhodu, že celé zařízení je hotové včetně krabičky.

Na zvolené HW bude napsán vlastní firmware v Arduino frameworku s následujícími vlastnostmi:

-   měření teploty pomocí teplotního čidla DS18B20, které bude připojenou přes UTP kabel cca 10 metrů
    -   čidlo bude připojeno přes již integrovaný port v zařízení RJ9 (komunikace přes 1-wire)
-   odesílání naměřené teploty do IoT Platformy každých 5 minut přes mqtt
-   odesilání informace zda je aktivní levný tarif
    -   bude měřeno přes HDO u elektroměru
    -   zařízení nemá žádný další dostupný port, takže bude nutná modifikace -> prodávají se verze s displejem, kde je použito stejné PCB, takže bude využit některý volný pin a vyveden konektor na který bude možné připojit spínač, který je spínán přes HDO. Předpokládám vyvedení dvou pinout - jeden GND a druhý pin ESP, který bude měřit stav
-   odolnost vůči výpadku wifi:
    -   při ztrátě signálu wifi se zařízení bude každých 5 min opětovně připojovat
    -   pokud nebude připojeno k wifi, tak bude bojler zapínán podle HDO

Následně bude zařízení nainstalováno a přes nástroj node-red bude nastavena logika pro zapínání bojleru v závislosti na teplotě a aktuální výroby elektrické energie fotovoltajickou elektrárnou.
Součástí bude přepsání Arduino knihovny s plným využitím asynchroních funkcí z wifiManageru - nyní využívám synchroní a dochází k příliš dlouhému blokování v některých případech.

## Soupis HW

-   čidlo DS18B20 ve vodotěsné variantě - bude jednoduší jeho upevnění na bojler, protože je větší než pouhý senzor samotný
-   Sonoff TH Origin 20A Switch Module (THR320)
-   UTP kabel 8m
-   RJ9 konektor + krimplovací kleště
-   pájecí stanice
-   pin lišta 1x2 pinů samec (zahlá 90°) a pin lišta 1x4 (přímá)
-   dvoupinový a čtyřpinový konektor samice na pin lištu
-   1m kabelu (pro měření stavu levného tarifu)
-   multimetr (potřeba změřit napětí)

## Implementace HW

### Úprava modulu Sonoff THR320

Nejprve bylo potřeba modul rozdělat a identifikovat nevyužité piny. Všechny modely Sonoff TH mají stejné PCB. Podíval jsem se tedy na rozpis pinů použitých pro [Sonoff TH Elite](https://templates.blakadder.com/sonoff_THR316D.html), které ma displej. Zvolil jsem pin `5`, který jsem nasatvil na HIGH a pomocí multimetru jsem identifikoval na PCB daný pin. Ověřil jsem, že není nikam jinam zapojení - měřením a vizuálně. PCB dále mělo další prokovené průchodu, do kterých verze Sonoff TH Elite má umístěné další malé relé - tyto vývody nebyli nikam zapojeny a tak jsem do nich umístil pin lištu 1x2, na jeden pin jsem přivedl pin `5` a na druhé GND. Do krabičky jsem v daném místě vyvrtal díru, tak abych mohl zapojit konektor. Toto mi umožní měření průchodnosti proudu mezi danými dvěma piny. Toto jsou všechny potřebné úpravy.

### Instalace HW

Sonoff dává k tomuto zařízení i plastový držák na DIN lištu a ten jsem využil pro přidělání přímo do rozvaděče, protože jsem potřeboval mít přímí přístup jako k HDO, tak i k napájení bojleru. Napájení bojleru do teď bylo spínáno pomocí spínače, který ovládá HDO a spíná fázi pro bojler. Sonoff zařízení jsem zapojil k napájení 230V a na output jsem připojil přívodní fázi k bojleru místo aby byla zapojena přes spínač ovládaný HDO - takto tedy bude moje zařízení zapínat a vypínat fázi k bojleru.

Pro detekci levného tarifu, jsem využil spínač ovládaný přes HDO, který před tím spínal přímo fázi bojleru, ale místo toho jsem to zapojil tak, aby spínal kontakt mezi mími dvěmi vyvedenými kontakty - nyní lze detekovat levný tarif podle toho zda na pinu `5` bude přivedeno GND či nikoli.

Mezi bojlerem a rozvaděčem byl natažený UTP kabel, pro komunikaci se senzorem teploty. Do izolace bojleru byla vyvrtána díra, do které byl vložen teplotní senzor a pomocí pružinky, bylo dosaženo konstatního přítlaku kvůli dodržení dostatečného kontaktu. Kabel a senzor byl propojen pomocí konektoru, aby bylo možné jednoduše senzor vyměnit v případě jeho selhání. Na druhou stranu UTP kabelu byl nakrimplován konektor RJ9, který byl zapojen do Sonoff zařízení.

## SW

Pro vývoj jsem využil Arduino framework a prostředí [PlatformIO](https://platformio.org/).

### Použité knihovny:

-   https://github.com/founek2/IoT-Platform-library integrace s IoT Platformou
-   https://github.com/milesburton/Arduino-Temperature-Control-Library komunikace s teplotním senzorem
-   Ticker z arduino arduinoespressif32, pro časování intervalu měření

### Funkcionality:

-   tlačítko
    -   krátký stisk přepne relátko
    -   dlouhý stisk resetuje zařízení do továrního nastavení
-   ledky
    -   červená indikuje stav relátka
    -   zelená indikuje stisk tlačítka
    -   modrá indikuje stav připojení k platformě
-   komunikace
    -   měření a odeslání teploty každých 5 minut
    -   reakce na příkaz k zapnutí/vypnutí relátka přes MQTT
    -   odesílání stavu relátka a levného tarifu
-   fallback
    -   v případě výpadku spojení s platformu zařízení automaticky zapíná bojler po dobu trvání levného tarifu

[Zdrojový kód](https://github.com/founek2/IoT-Platform-library/tree/master/examples/bojler)

# Arduino knihovna

## Zadání

Aktuální verze Arduino knihovny pro integraci s IoT Platformou blokuje loop až na několik minut v určitých případech. Dále při nedostupnosti Wi-Fi připojení knihovna spadne (hodí exception) a způsobí pád celé aplikace a následný restart zařízení. Je tedy potřeba přepsat knihovnu, aby využila asynchronní funkce WifiManageru, který nyní způsobuje dlouhé blokování, a současně podporovala běh při nedostupni Wi-Fi či internetového připojení a opětovné připojení.

## SW

Implementované funkce:

-   nyní jsou plně využity asynchronní operace knihovny WifiManager a nedochází tak k dlouhému blokování voláním
-   zachována funkčnost i při nedostupnosti Wi-Fi / internetu:
    -   pokus o opětovné připojení k Wi-Fi každých 5 minut
    -   pokus o opětovné připojení k MQTT brokeru každých 40s
    -   detekce nedostupnosti platformy a vystavení této informace přes rozhraní
    -   po opětovném připojení odeslání aktuálních hodnot
-   nově podpora esp32

[Zdrojový kód](https://github.com/founek2/IoT-Platform-library)
