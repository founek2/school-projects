text = """
 # Edward Aloysius Murphy Jr.
 Jestliže si udržuješ klid, když všichni ostatní ztrácí hlavu, je to tím, že jsi nepochopil podstatu problému.
 Tajemstvím úspěchu je upřímnost. Až se i tu naučíš předstírat, máš kariéru zaručenu.
 Žádný šéf si neponechá zaměstnance, který má stále pravdu.
 Kdo něco umí, ten to dělá. Kdo to neumí, ten to učí. Kdo to neumí učit, ten to řídí.
 Zákon zkoušení: Co žák neumí, učitel zjistí během 1 minuty, a co umí, ho nezajímá.
 Krást myšlenky od jedné osoby je plagiátorství. Krást myšlenky od mnoha lidí je výzkum.
 Když je něco dobré, přestanou to dělat.
 Jestli se cítíš dobře, uklidni se, ono tě to přejde.
 Nikdo neposlouchá, co říkáte, dokud neuděláte chybu.
"""

def abc(xs):
 for x in xs.strip().split('\n'):
    if x.startswith('#'):
        continue
    yield x.strip()


if __name__ == "__main__":
    xs = set()
    #print(next(abc(text)))
    for i, x in enumerate(abc(text)):
        print(i, x)
        if i % 2 == 1:
            xs.add(x.count('.'))
    print(sum(xs))