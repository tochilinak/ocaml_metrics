<table>
    <thead>
        <tr>
            <th>Metrics group</th>
            <th>Metrics</th>
            <th>Original description</th>
            <th>Notes</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan=5>Cohesion</td>
            <td>LCOM1</td>
            <td>Number of pairs of methods that do not share attributes</td>
            <td></td>
        </tr>
        <tr>
            <td>LCOM2</td>
            <td>P = Number of pairs of methods that do not share attributes<br />
                Q = Number of pairs of methods that share attributes<br />
                LCOM2 = max(P - Q, 0)</td>
            <td></td>
        </tr>
        <tr>
            <td>LCOM3</td>
            <td>Number of disjoint components in the graph that represents each method as a<br />
                node and the sharing of at least one attribute as an edge</td>
            <td></td>
        </tr>
        <tr>
            <td>LCOM4</td>
            <td>Similar to LCOM3 and additional edges are used to represent method invocations</td>
            <td></td>
        </tr>
        <tr>
            <td>LCOM5</td>
            <td></td>
            <td></td>
        </tr>
    </tbody>
</table>
