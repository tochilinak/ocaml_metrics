### В целом

1. Если работал препроцессор, ничего не посчитается. Сейчас в My_Tast_iterator я сделала так, что в structure_item с пометкой ghost я просто не хожу. Поэтому, например, в файле metrics/test.ml ничего вообще не находится.

2. Для сопоставления модуля файлу, я беру из LoadDune.ml `Cmt_format.cmt_modname`, а потом разделяю имя по `__`. Если в названии файла есть `__`, это отработает неправильно.

3. Чтобы обрабатывать ситуации, когда объявление функции имеет вид `let a, b = "a", "b"`, у меня названия функций берутся в двух форматах --- в виде строкового представления и в виде списка идентификаторов. Но я рассматриваю только случаи перечисления имен через запятую, наверняка бывают и другие ситуации.

4. В LoadDune я рассматриваю только такие Executables, у которых в списке names только один элемент. Из того, что я видела, обычно так и есть, но контрпример есть, например, тут: https://github.com/backtracking/ocamlgraph/tree/master/examples. Такое я не рассматриваю, потому что не очень понимаю, как тут сопоставлять имя исполняемого файла и модуль.

### Halstead

Операнды: константы, идентификаторы (которые не являются операторами), конструкторы без аргументов, поля записей, шаблон `_`.

Операторы: идентификаторы стоящие в левой части texp_apply или @@; конструкторы с аргументами; узлы синтаксического дерева, не попадающие в ранее перечисленные категории.

1. Понятно, что я не все ситуации рассматривала. Как минимум еще есть variant, letop.
   
   - У меня в main.ml в `groups_of_metrics` каждый перечисленный модуль не считается отдельным операндом.
   - В выражении `(module L : METRIC.GROUP)` `L` считается дважды (там из дерева понятно, почему так).

2. По поводу конструкторов с аргументами неочевидно, куда их стоит относить. Потому что, например, `::` похож на оператор, а `Some` не очень (а может и нормально).

### Coupling

1. Тут есть разделение на 3 типа модулей:
   - публичные
   - приватные
   - анонимные

   Граф строится только для публичных модулей. По графу считаются Fan-in, Fan-out, APIU, поэтому они есть только для публичных модулей. В целом, Fan-out можно было бы считать для всех, но тогда его надо считать не по графу.

2. Эта группа метрик самая медленная, поэтому если надо будет ускорять, то в первую очередь этот модуль.

3. Бывает ситуация (видимо редкая), когда один и тот же глобальный модуль объявлен 2 раза. Например, такое есть в версии dune 2.8.0 в файлах src/memo/memo.mli и src/memo/run.ml (модуль Memo.Run). Вроде конкретно в данном случае моя программа обрабатывает даже корректно, но я не знаю, как это разрешается в общем случае.

4. Я не обрабатываю такие ситуации (и не уверена, надо ли):

        module X = Y.Z
        X.run ()

   (run скорее всего не посчитается вызовом из модуля Y.Z)

5. Если есть публичные модули A и A.B, и A вызывает у A.B (или наоборот) непубличную функцию, то при подсчете у A метрики APIU модуль A.B игнорируется (из-за таких ситуаций в `Edge_label` есть поле `consider_in_apiu`).
6. По определениям метрики EXT не очень понятно, что обычно имеют в виду: количество вызовов внешних функций или количество различных внешних функций. У меня считается второй вариант. Первый вариант считался бы быстрее.