Nota de pesquisa I: Relação entre escolha profissional e estágio supervisionado
================

Introdução
----------

Manipulação dos dados
---------------------

``` r
# Carrega bibliotecas
library(dplyr)  # Manipulação de dados
library(descr)  # Tabulação Cruzada
library(plyr)   # Mudanaça de valores
library(rmarkdown) # Escrever relatório
library(pander)  # Escrever relatório
```

``` r
# Cria data frame chi com colunas do questionário para lic
chi <- fis14 %>%
  tbl_df() %>%
  select(qe_i68:qe_i81)

# Agrupa valores de estágio e cria coluna 73b
chi$qe_i73b<-revalue(chi$qe_i73, c("c"="b", "d"="b", "e"="c", "f"="d"))
```

Tabulação e teste
-----------------

``` r
c3 <- CrossTable(chi$qe_i69, chi$qe_i73b, chisq=TRUE, 
                 expected = TRUE, format = "SPSS",
                 dnn=c("item 69", "item 73"))
```

    ## Warning in chisq.test(tab, correct = FALSE, ...): Chi-squared approximation
    ## may be incorrect

``` r
pander(c3, caption = "Tabela Cruzada - itens 69 e 73 - querer exercer o magistério vs local de estágio")
```

<table style="width:86%;">
<caption>Tabela Cruzada - itens 69 e 73 - querer exercer o magistério vs local de estágio (continued below)</caption>
<colgroup>
<col width="18%" />
<col width="18%" />
<col width="16%" />
<col width="16%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> <br />
item 69</th>
<th align="center">item 73<br />
a</th>
<th align="center"> <br />
b</th>
<th align="center"> <br />
c</th>
<th align="center"> <br />
d</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>a</strong><br />
N<br />
<br />
Chi-square<br />
Row(%)<br />
Column(%)<br />
Total(%)</td>
<td align="center"> <br />
1449<br />
1438.8438<br />
0.0717<br />
89.3342%<br />
56.5795%<br />
50.1905%</td>
<td align="center"> <br />
116<br />
97.1964<br />
3.6377<br />
7.1517%<br />
67.0520%<br />
4.0180%</td>
<td align="center"> <br />
10<br />
15.7312<br />
2.0880<br />
0.6165%<br />
35.7143%<br />
0.3464%</td>
<td align="center"> <br />
47<br />
70.2286<br />
7.6830<br />
2.8977%<br />
37.6000%<br />
1.6280%</td>
</tr>
<tr class="even">
<td align="center"><strong>b</strong><br />
N<br />
<br />
Chi-square<br />
Row(%)<br />
Column(%)<br />
Total(%)</td>
<td align="center"> <br />
571<br />
576.6020<br />
0.0544<br />
87.8462%<br />
22.2960%<br />
19.7783%</td>
<td align="center"> <br />
28<br />
38.9505<br />
3.0786<br />
4.3077%<br />
16.1850%<br />
0.9699%</td>
<td align="center"> <br />
10<br />
6.3041<br />
2.1668<br />
1.5385%<br />
35.7143%<br />
0.3464%</td>
<td align="center"> <br />
41<br />
28.1434<br />
5.8732<br />
6.3077%<br />
32.8000%<br />
1.4202%</td>
</tr>
<tr class="odd">
<td align="center"><strong>c</strong><br />
N<br />
<br />
Chi-square<br />
Row(%)<br />
Column(%)<br />
Total(%)</td>
<td align="center"> <br />
163<br />
169.4323<br />
0.2442<br />
85.3403%<br />
6.3647%<br />
5.6460%</td>
<td align="center"> <br />
12<br />
11.4454<br />
0.0269<br />
6.2827%<br />
6.9364%<br />
0.4157%</td>
<td align="center"> <br />
4<br />
1.8524<br />
2.4897<br />
2.0942%<br />
14.2857%<br />
0.1386%</td>
<td align="center"> <br />
12<br />
8.2698<br />
1.6825<br />
6.2827%<br />
9.6000%<br />
0.4157%</td>
</tr>
<tr class="even">
<td align="center"><strong>d</strong><br />
N<br />
<br />
Chi-square<br />
Row(%)<br />
Column(%)<br />
Total(%)</td>
<td align="center"> <br />
378<br />
376.1219<br />
0.0094<br />
89.1509%<br />
14.7599%<br />
13.0932%</td>
<td align="center"> <br />
17<br />
25.4077<br />
2.7822<br />
4.0094%<br />
9.8266%<br />
0.5888%</td>
<td align="center"> <br />
4<br />
4.1122<br />
0.0031<br />
0.9434%<br />
14.2857%<br />
0.1386%</td>
<td align="center"> <br />
25<br />
18.3582<br />
2.4030<br />
5.8962%<br />
20.0000%<br />
0.8660%</td>
</tr>
<tr class="odd">
<td align="center">Total<br />
</td>
<td align="center">2561<br />
88.708%</td>
<td align="center">173<br />
5.9924%</td>
<td align="center">28<br />
0.9699%</td>
<td align="center">125<br />
4.3298%</td>
</tr>
</tbody>
</table>

<table style="width:15%;">
<colgroup>
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> <br />
Total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"> <br />
1622<br />
<br />
<br />
56.1829%<br />
<br />
</td>
</tr>
<tr class="even">
<td align="center"> <br />
650<br />
<br />
<br />
22.5147%<br />
<br />
</td>
</tr>
<tr class="odd">
<td align="center"> <br />
191<br />
<br />
<br />
6.6159%<br />
<br />
</td>
</tr>
<tr class="even">
<td align="center"> <br />
424<br />
<br />
<br />
14.6865%<br />
<br />
</td>
</tr>
<tr class="odd">
<td align="center">2887<br />
</td>
</tr>
</tbody>
</table>

Gráfico
-------

Considerações finais
--------------------
