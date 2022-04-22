Metrics names are printed in the following format: `<metrics group id>_<metrics id>`.

<table align="center">
    <thead>
        <tr>
            <th>Metrics group id</th>
            <th>Description</th>
            <th>Metrics id</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody align="center">
        <tr>
            <td rowspan=5>cohesion</td>
            <td rowspan=5>
                <b>Class cohesion metrics</b><br />
                Alternative to class is a module.<br />
                Alternative to both attributes and methods are functions.<br />
                "Methods share attributes" -> "Functions call the same function or one of them calls the other"
            </td>
            <td align="center">LCOM1</td>
            <td><i>Original definition:</i> Number of pairs of methods that do not share attributes</td>
        </tr>
        <tr>
            <td>LCOM2</td>
            <td><i>Original definition:</i><br />
                P = Number of pairs of methods that do not share attributes<br />
                Q = Number of pairs of methods that share attributes<br />
                LCOM2 = max(P - Q, 0)</td>
        </tr>
        <tr>
            <td>LCOM34</td>
            <td>LCOM3 = Number of disjoint components in the graph that represents each method as a
                node and the sharing of at least one attribute as an edge <br />
                LCOM4 = Similar to LCOM3 and additional edges are used to represent method invocations <br />
                <i>In our interpretation LCOM3 and LCOM4 give the same values.</i>
            </td>
        </tr>
        <tr>
            <td>LCOM5</td>
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
                in graph where nodes are functions and edges are used to represent one function calling another.
            </td>
        </tr>
        <tr>
            <td>COH</td>
            <td> <i>Original definition:</i>
                 <img src="https://render.githubusercontent.com/render/math?math=\frac{a}{kl}">, where 
                 <img src="https://render.githubusercontent.com/render/math?math=a">,
                 <img src="https://render.githubusercontent.com/render/math?math=k">, and
                 <img src="https://render.githubusercontent.com/render/math?math=l">
                 have the same definitions as above <br />
                <i>Our interpretation:</i> Similar to LCOM5.
            </td>
        </tr>
    </tbody>
</table>
