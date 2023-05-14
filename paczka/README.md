# Interpreter

## Opis projektu

Projekt jest interpreterem języka imperatywnego. Składnia języka jest opisana przez gramatykę `gramatyka.cf`. Parser został wygenerowany za pomocą narzędzia BNFC. Interpreter został napisany w języku Haskell.

### Opis plików

- `src/Interpreter.hs` - główny moduł interpretera, wczytuje plik z kodem źródłowym, parsuje go, sprawdza typowanie i wykonuje.
- `src/TypeChecker.hs` - moduł odpowiedzialny za statyczne sprawdzanie typów. Sprawdza także redeklaracje zmiennych globalnych, redeklaracje funkcji, redeklaracje parametrów funkcji, ilość argumentów funkcji w wywołaniu, obecność funkcji `main()`, czy używane zmienne i funkcje są zadeklarowane.
- `src/Eval.hs` - moduł odpowiedzialny za wykonanie programu. Obsługuje błedy wykonania takie jak dzielenie przez 0.

## Kompilacja

Wystarczy wykonać polecenie `make` w głównym katalogu projektu.

## Testy

Rozwiązanie zawiera skrypt testujący `test.sh`. Aby uruchomić testy można wykonać polecenie `./test.sh` lub `make test` w głównym katalogu projektu.

## Opis języka

Jest to język imperatywny. Składnia jest wzorowana na składni Latte.

### Gramatyka

Składnia języka jest opisana przez gramatykę `gramatyka.cf`.

Gramatyka zawiera jeden konflikt typu `shift-reduce` związany z instrukcją `if-else`. W przypadku konfliktu parser wykonuje `shift`, co oznacza, że `else` łączy się z najbliższym `if`.

### Typy

W języku są typy `int`, `bool`, `string`. Nie ma konwersji pomiedzy typami. Typy mają wartości domyślne (`0` dla `int`, `""` dla `string`, `false` dla `bool`).

### Program

Program jest listą definicji funkcji i zmiennych globalnych. W programie musi wystąpić funkcja o nazwie main zwracająca int i nie przyjmująca argumentów. Wykonanie programu polega na zainicjalizowaniu zmiennych globalnych, a następnie wykonaniu funkcji main.

Funkcje i zmienne globalne mogą być definiowane w dowolnej kolejności (użycie funkcji lub zmiennej globalnej może występować przed jej definicją).

### Funkcje

Na definicję funkcji składa się typ zwracanej wartości, nazwa, lista argumentów oraz ciało. Funkcje muszą mieć unikalne nazwy. Jeżeli funkcja nie ma instrukcji return zwracana jest wartość domyślna. Parametry funkcji są przekazywane przez wartość lub przez zmienną.

### Zmienne globalne

Zmienne globalne są widoczne wszędzie, w szczególności mogą być użyte w funkcji zdefiniowanej przed deklaracją zmiennej globalnej. Zmienne globalne muszą mieć unikalne nazwy. Zmienne globalne są inicjalizowane na początku wartością domyślną, a następnie wartością wyrażenia inicjalizującego, w kolejności deklaracji.

### Zmienne lokalne

Jeśli zmienna (globalna lub lokalna) nie jest jawnie inicjalizowana w momencie deklaracji, jest inicjalizowana wartością domyślną. Deklaracje zmiennych lokalnych mogą występować w dowolnym miejscu bloku, jednak każda zmienna musi być zadeklarowana przed użyciem. Zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie spoza bloku. Redeklaracja zmiennej w bloku przesłania poprzednią.


### Funkcje wbudowane

Są dostępne następujące funkcje wbudowane:
- `int printInt(int)`
- `int printString(string)`
- `int printBool(bool)`
- `int printLnInt(int)`
- `int printLnString(string)`
- `int printLnBool(bool)`

## Tabelka cech
```
Na 15 punktów
01 (trzy typy)                                                     +
02 (literały, arytmetyka, porównania)                              +
03 (zmienne, przypisanie)                                          +
04 (print)                                                         +
05 (while, if)                                                     +
06 (funkcje lub procedury, rekurencja)                             +
Jedna rzecz z poniższej listy lub coś o porównywalnej trudności:
07 (przez zmienną / przez wartość / in/out)                        + (przez zmienną / przez wartość)
08 (zmienne read-only i pętla for)

Na 20 punktów
09 (przesłanianie i statyczne wiązanie)                            +
10 (obsługa błędów wykonania)                                      +
11 (funkcje zwracające wartość)                                    +

Na 30 punktów
12 (4) (statyczne typowanie)                                       +
13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
15 (2) (krotki z przypisaniem)
16 (1) (break, continue)
17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
18 (3) (generatory)
```
**Razem: 24**

## Uwagi

Interpreter wywołany z opcją `-v` wypisuje dodatkowe iformacje na `stderr`.

Testy w katalogu `bad/` są uruchamiane z opcją `-v`, natomiast testy w katalogu `good/` bez.
