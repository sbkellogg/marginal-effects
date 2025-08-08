## \begin{tikzpicture}
##     \node (W1) at (0, -1) {$Z$};
##     \node (W2) at (1, -1) {$W$};
##     \node (X) at (1, 0) {$X$};
##     \node (Y) at (2, 0)  {$Y$};
##     \draw[->] (X) -- (Y);
##     \draw[->] (W1) to[out=-45, in=-45] (Y);
##     \draw[->] (W2) -- (X);
##     \draw[->] (W1) -- (W2);
##     \draw[->] (W2) -- (Y);
## \end{tikzpicture}

## ```{r}
## #| engine: tikz
## #| echo: false
## #| label: fig-dag
## #| fig-cap: Example of a Directed Acyclic Graph
## #| out-width: 30%
## \begin{tikzpicture}
##     \node (W1) at (-2, -1) {$Z$};
##     \node (W2) at (-3, -1) {$Z$};
##     \node (X) at (-1, 0) {$X$};
##     \node (Y) at (1, 0)  {$Y$};
##         \draw[->] (0,0) to[out=45,in=135] (2,0);
##     \draw[->] (C) -- (T);
##     \draw[->] (C) -- (O);
##     \draw[->] (T) -- (O);
## \end{tikzpicture}
## ```
