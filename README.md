# Лабораторной работа №3

> Выполнила: Васина Дарья P3312

## Постановка задачи

Разработать программу, осуществляющую интерполяцию функции по заданным точкам. 

#### Требования

- необходимо поддерживать работу с несколькими алгоритмами интерполяции одновременно
- расчет интерполяции должен производиться в оконном режиме по мере получения данных
- потоковая обработка входных данных из stdin с выводом в stdout
- требуется динамически осуществлять расчет и вывод новых значений сразу после получения достаточного количества входных данных
- для каждого метода необходимо выводить таблицу расчета значений интерполяционной функции в тестовых точках с фиксированным шагом на данном интервале
- cli для настройки режима работы
- алгоритмы: линейная интерполяция, многочлен Лагранжа

## Реализация

Структура проекта:

- `main.ml` - точка входа в приложение
- `interpolation.ml` - реализация линейной интерполяции и интерполяции по Лагранжу
- `io.ml`: ввод/вывод данных

## CLI
- Метод указывается через аргумент `-m`. Доступные методы: `lagrange`, `linear`. 
- Шаг дискретизации указывается через аргумент `-s`.

## Пример работы приложения

$ dune exec ./bin/main.exe -- -m linear -s 0.5

```

Введите точку (X Y через пробел):  
0 0
Введите точку (X Y через пробел):
1.5 1
0.00    1.00    2.00
0.00    0.67    1.00
Введите точку (X Y через пробел):
3.142 0
1.50    2.50    3.50
1.00    0.39    0.00
Введите точку (X Y через пробел):
4.7 -1
3.14    4.14    5.14
0.00    -0.64   -1.00
```


