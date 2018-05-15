* Na 14 punktów
+ 1 (dwa typy) [good/types.xul]
+ 2 (arytmetyka, porównania) [good/arithmetics.xul, good/comparisons.xul]
+ 3 (while, if) [good/loops.xul, good/cond.xul]
+ 4 (procedury lub funkcje, rekurencja) [good/functions.xul]
+ 5 (print) [good/print.xul]
  6 a) n/a
+   b) (pętla for) [good/loops.xul]
+   c) (string i rzutowania) [good/string.xul]
    d) n/a
  Na 17 punktów
+ 7 (statyczne typowanie) [bad/types*.xul, bad/shadowing*.xul]

Inne pliki z poprawnymi przykładami:
- good/big_example.xul: większość funkcji języka, wysłany z deklaracją
- good/arg.xul: przekazywanie argumentów do programu
- good/shadowing.xul: przesłanianie identyfikatorów (można skompilować GCC)

Proponowana ocena: 17 punktów, spełnione warunki z treści zadania

# Zmiany: W poniższej specyfikacji nie doszło do żadnych istotnych zmian odkąd
# została wysłana do oceny miesiąc temu. Został jedynie doprecyzowany sposób
# przekazywania argumentów do programu.
# Plik Xul.cf z gramatyką w ogóle nie został zmieniony.

Język Xul to statycznie typowany język, który bazuje na Latte
i pożycza z niego większość podstawowych cech.
Gramatyka w formacie BNFC znajduje się w dołączonym pliku Xul.cf,
lub skompilowana do HTML w DocXul.html.
Przykładowy działający program jest w pliku example.xul,
powinien wyczerpywać wszystkie potencjalnie nieoczywiste konstrukcje.
W pliku example.out jest przewidywane wyjście tego programu.

Podobnie jak w Latte, program jest listą definicji funkcji o unikalnych nazwach,
które muszą albo mieć typ void, albo zwrócić wartość za pomocą return.
Funkcje typu void mogą wykonać return bez parametru aby wymusić zakończenie.
Definicje funkcji wyglądają tak jak w C/Latte.
Funkcje mogą być rekurencyjne.
Argumenty do funkcji przekazywane są przez wartość.

W programie musi znajdować się funkcja main zwracająca int, od której zaczyna
się wykonanie programu. Funkcja ta może nie przyjmować żadnych argumentów
lub przyjąć jeden argument typu string.
Argument do programu przekazujemy poprzez wywołanie interpretera z flagą
`--arg=argument`. Przekazanie argumentu do programu, którego funkcja main nie
przyjmuje argumentów, spowoduje jego zignorowanie. Nieprzekazanie argumentu
do programu, którego funkcja main takiego argumentu oczekuje, spowoduje
przekazanie do niego pustego stringa ("").

Dostępne typy to: int, bool, void, string.

Zmienne deklaruje się za pomocą `typ nazwa = wyrażenie`, w szczególności
nie jest możliwe zadeklarowanie zmiennej bez podania jej początkowej wartości.

Zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o
tej samej nazwie spoza bloku. W obrębie bloku zmienne muszą mieć unikalne nazwy.

Zmienna po lewej stronie = w instrukcji przypisania musi być tego samego typu
co wyrażenie po jego prawej stronie.
Wszystkie operatory (AddOp, MulOp, RelOp) działają tylko na zmiennych
tych samych typów.
W instrukcjach if(e) oraz while(e), e musi być typu bool.

Instrukcje ++, -- działają tylko na zmiennych typu int, podobnie jak operatory
-, *, /, %.

Na zmiennych typu int oraz string można dodatkowo użyć operatorów
+, <, <=, >, >=. Na stringach operator + to konkatenacja,
porównania są leksykograficzne.

==, != można używać na zmiennych wszystkich typów.

Dodatkowo na intach można użyć jednoargumentowego operatora -a
oraz na boolach operatora !a.

Operacje arytmetyczne i boolowskie oraz ich priorytety są standardowe
i opisane w gramatyce.

Pętla for jest w stylu mieszającym C/Python/Pascal, przykładowo
  for (int a in b to c) {/* ... */}
wykona ciało pętli dla każdej wartości a w zbiorze {b, b+1, ..., c-1, c}.
Analogicznie można zrobić downto dla warotści {b, b-1, ..., c+1, c}.

Wbudowana instrukcja print przyjmuje dowolną liczbę argumentów
dowolnych (potencjalnie różnych) typów i wypisuje je oddzielone spacjami,
zakończone '\n'. W szczególności print() wypisuje sam znak nowej linii.

Komentarze jednoliniowe mogą zaczynać się # lub //, a komentarze blokowe
powinny zawierać się w /* ... */.

Istnieją wbudowane funkcje:
  string intToString(int)
  int stringToInt(string)
gdzie wywołanie stringToInt z argumentem niebędącym tekstową reprezentacją
liczby powoduje zakończenie programu nieokreślonym błędem.
