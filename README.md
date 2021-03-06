# Ocaml metrics

Run tool:

    ocaml_metrics <DIR> [-met-list <M1>,<M2>,<M3>] [-sec-list <L1>,<L2>,<L3>] [-v-list <V1>,<V2>,<V3>]

`<DIR>` - directory with dune-based project

`-met-list <M1>,<M2>,<M3>` - metrics to show (metrics is shown if it contains a substring from this list)

`-sec-list  <L1>,<L2>,<L3>` - dune libraries and executables from project to analyze

`-v-list <V1>,<V2>,<V3>` - print verbose output of metrics groups from this list

Metrics names are printed in the following format: `[metrics group id]_[metrics id]`.

<table align="center">
    <thead>
        <tr>
            <th>Metrics group id</th>
            <th width="30%">Description</th>
            <th>Metrics id</th>
            <th width="50%">Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan=5 align="center" >cohesion</td>
            <td rowspan=5 align="left">
                <b align="center">Class cohesion metrics</b><br />
                Alternative to class is a module.<br />
                Alternative to both attributes and methods are functions.<br />
                "Methods share attributes" -> "Functions call the same function or one of them calls the other"
            </td>
            <td align="center">LCOM1</td>
            <td  align="left" ><i>Original definition:</i> Number of pairs of methods that do not share attributes</td>
        </tr>
        <tr>
            <td  align="center">LCOM2</td>
            <td><i>Original definition:</i><br />
                P = Number of pairs of methods that do not share attributes<br />
                Q = Number of pairs of methods that share attributes<br />
                LCOM2 = max(P - Q, 0)</td>
        </tr>
        <tr>
            <td  align="center">LCOM34</td>
            <td>LCOM3 = Number of disjoint components in the graph that represents each method as a
                node and the sharing of at least one attribute as an edge <br />
                LCOM4 = Similar to LCOM3 and additional edges are used to represent method invocations <br />
                <i>In our interpretation LCOM3 and LCOM4 give the same values.</i>
            </td>
        </tr>
        <tr>
            <td  align="center">LCOM5</td>
            <td>
                <i>Original definition:</i>
                <img src="https://render.githubusercontent.com/render/math?math=\frac{a-kl}{l-kl}">, where
                <img src="https://render.githubusercontent.com/render/math?math=l">
                is the number of attributes,
                <img src="https://render.githubusercontent.com/render/math?math=k">
                is the number of methods, and
                <img src="https://render.githubusercontent.com/render/math?math=a">
                is the summation of the number of distinct attributes accessed by each method in a class <br />
                <i>Our interpretation:</i>
                Instead of
                <img src="https://render.githubusercontent.com/render/math?math=kl"> we use maximum possible number of edges
                in graph where nodes are functions and edges are used to represent one function calling another. <br/>
                When denominator is >= 0, LCOM5 is -1.
            </td>
        </tr>
        <tr>
            <td  align="center">COH</td>
            <td> <i>Original definition:</i>
                 <img src="https://render.githubusercontent.com/render/math?math=\frac{a}{kl}">, where 
                 <img src="https://render.githubusercontent.com/render/math?math=a">,
                 <img src="https://render.githubusercontent.com/render/math?math=k">, and
                 <img src="https://render.githubusercontent.com/render/math?math=l">
                 have the same definitions as above <br />
                <i>Our interpretation:</i> Like in LCOM5.
            </td>
        </tr>
        <tr>
            <td rowspan=5 align="center">coupling</td>
            <td rowspan=5 align="left">
                <b align="center">Coupling metrics</b><br />
                <i>Note:</i> Fan-in, Fan-out and APIU are calculated only for public modules;<br />
                EXT is calculated for both modules and functions.
            </td>
            <td align="center">Fan-in</td>
            <td align="left">Number of modules that depend on given module </td>
        </tr>
        <tr>
            <td  align="center">Fan-out</td>
            <td>
                Numbers of modules that the given module depends on
            </td>
        </tr>
        <tr>
            <td  align="center">APIU</td>
            <td>
                API Function Usage Index.<br />
                Suppose that module
                <img src="https://render.githubusercontent.com/render/math?math=m">
                has
                <img src="https://render.githubusercontent.com/render/math?math=n">
                public functions,
                in project <img src="https://render.githubusercontent.com/render/math?math=k">
                other modules <img src="https://render.githubusercontent.com/render/math?math=m_1, \ldots, m_k">
                call one or more function from <img src="https://render.githubusercontent.com/render/math?math=m">.<br />
                <img src="https://render.githubusercontent.com/render/math?math=\mathrm{APIU} = \frac{\sum_{j=1}^k n_j}{nk}">
                or 0 if <img src="https://render.githubusercontent.com/render/math?math=nk = 0">
            </td>
        </tr>
        <tr>
            <td  align="center">EXT</td>
            <td>
                External method calls.<br />
                Number of external functions called by a function or a module (only functions defined in the project are considered).
            </td>
        </tr>
        <tr>
            <td  align="center">AC</td>
            <td>
                Association-Induced Coupling Metrics.<br />
                M = modules in project;<br />
                Mod_ext(m) = modules that have functions that use functions from m;<br />
                F = functions in project;<br />
                F(m) = functions in module m;<br />
                Func_ext(m) = functions that use functions from m;<br />
                Func_int(m) = functions in m that use functions from other modules in M;
                <br />
                <img src="https://render.githubusercontent.com/render/math?math=\mathrm{AC}_1(m) = 1 - \frac{|\mathrm{Mod_ext}(m)|}{|M| - 1}"><br />
                <img src="https://render.githubusercontent.com/render/math?math=\mathrm{AC}_2(m) = 1 - \frac{|\mathrm{Func_ext}(m)|}{|F| - |F(m)|}"><br />
                <img src="https://render.githubusercontent.com/render/math?math=\mathrm{AC}_3(m) = 1 - \frac{|\mathrm{Func_int}(m)|}{|F(m)|}"><br />
                <img src="https://render.githubusercontent.com/render/math?math=\mathrm{AC} = \min(\mathrm{AC}_1, \mathrm{AC}_2, \mathrm{AC}_3)">
            </td>
        </tr>
        <tr>
            <td rowspan=5  align="center">CC-based</td>
            <td rowspan=5>
                <b>McCabe's cyclomatic complexity based</b><br />
                Read more about CC: https://en.wikipedia.org/wiki/Cyclomatic_complexity
            </td>
            <td align="center">ord</td>
            <td>
                Traditional cyclomatic complexity
            </td>
        </tr>
        <tr>
            <td align="center">rec</td>
            <td>
                CC-ord + [Number of recursive calls]
            </td>
        </tr>
        <tr>
            <td align="center">mod</td>
            <td>
                Modified cyclomatic complexity. <br/>
                Like CC-ord but count <br/>
                <code> match E0 with | P1 -> E1 ... | PN -> EN </code> <br/>
                as 1 decision point instead of (N - 1) (but still count guards).
            </td>
        </tr>
         <tr>
            <td align="center">mod-ord-max</td>
            <td>
                Maximum of CC-based_ord in module
            </td>
        </tr>
        <tr>
            <td align="center">mod-ord-avg</td>
            <td>
                Average of CC-based_ord in module
            </td>
        </tr>
        <tr>
            <td rowspan=5 align="center" >Halstead</td>
            <td rowspan=5>
                <b> Halstead complexity measures </b> <br/>
                n1 = the number of distinct operators <br/>
                n2 = the number of distinct operands <br/>
                N1 = the total number of operators <br/>
                N2 = the total number of operands
            </td>
            <td align="center">n</td>
            <td>
                Program vocabulary:
                n1 + n2
            </td>
        </tr>
        <tr>
            <td align="center">N</td>
            <td>
                Program length:
                N1 + N2
            </td>
        </tr>
        <tr>
            <td align="center">V</td>
            <td>
                Volume:
                N * log2(n)
            </td>
        </tr>
        <tr>
            <td align="center">D</td>
            <td>
                Difficulty:
                (n1 / 2) * (N2 / n2)
            </td>
        </tr>
        <tr>
            <td align="center">E</td>
            <td>
                Effort:
                V * D
            </td>
        </tr>
        <tr>
            <td rowspan=2  align="center">LOC-based</td>
            <td rowspan=2>
                <b> Lines of code </b> <br/>
            </td>
            <td align="center">all</td>
            <td>
                Number of lines in function
            </td>
        </tr>
        <tr>
            <td align="center">code</td>
            <td>
                Exclude comment and blank lines
            </td>
        </tr>
    </tbody>
</table>
