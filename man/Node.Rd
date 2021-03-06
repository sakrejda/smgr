% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{Node}
\alias{Node}
\title{R6 class representing a single node, with links to child nodes}
\description{
R6 class representing a single node, with links to child nodes

R6 class representing a single node, with links to child nodes
}
\details{
The node has a name, an environment that contains its attributes, 
and a NodeSet that contains its children, along with accessors for
required functionality (determined by downstrea).
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{id}}{node id as string}

\item{\code{children}}{NodeSet of child nodes}

\item{\code{child_ids}}{character vector of child ids}

\item{\code{attributes}}{node attributes}

\item{\code{data}}{node data}

\item{\code{dump}}{contents as a list}

\item{\code{json}}{node contents as JSON}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Node$new()}}
\item \href{#method-matches}{\code{Node$matches()}}
\item \href{#method-mutate}{\code{Node$mutate()}}
\item \href{#method-spawn}{\code{Node$spawn()}}
\item \href{#method-merge}{\code{Node$merge()}}
\item \href{#method-disown}{\code{Node$disown()}}
\item \href{#method-transform}{\code{Node$transform()}}
\item \href{#method-modify}{\code{Node$modify()}}
\item \href{#method-get}{\code{Node$get()}}
\item \href{#method-has}{\code{Node$has()}}
\item \href{#method-clone}{\code{Node$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Used as Node$new(...) create a new node
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$new(name, ..., .children = NodeSet$new())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{name of the node (arbitrary label, not checked).}

\item{\code{...}}{an attributes to record (in the environment).}

\item{\code{.children}}{optoinally a list of child nodes (as NodeSet)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a 'Node' object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-matches"></a>}}
\if{latex}{\out{\hypertarget{method-matches}{}}}
\subsection{Method \code{matches()}}{
Check whether this node matches conditions specified in dots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$matches(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{interpreted as in `dplyr::filter` against the node
attributes.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
TRUE iff the tests pass
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-mutate"></a>}}
\if{latex}{\out{\hypertarget{method-mutate}{}}}
\subsection{Method \code{mutate()}}{
Modify node attributes as specified in dots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$mutate(..., .which = "attributes")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{interpreted as in `dplyr::mutate` against the node
attributes using a data mask and quosures.}

\item{\code{.which}}{either 'attributes' in which case the static node
attributes are modified and a new node ID will be generated 
or 'data' (anything else really) and the mutable data in the 
node will be modified.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
modified version of self
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-spawn"></a>}}
\if{latex}{\out{\hypertarget{method-spawn}{}}}
\subsection{Method \code{spawn()}}{
Deep-clone the node and mutate node attributes as specified in dots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$spawn(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{interpreted as in `dplyr::mutate` against the node
attributes using a data mask and quosures.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
modified version of the cloned node
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-merge"></a>}}
\if{latex}{\out{\hypertarget{method-merge}{}}}
\subsection{Method \code{merge()}}{
Merge another node's child id's into this node.  Only makes sense on
a node with identical id's... should test that,
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$merge(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{another Node object.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
modified self
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-disown"></a>}}
\if{latex}{\out{\hypertarget{method-disown}{}}}
\subsection{Method \code{disown()}}{
Clear record of child nodes, internal use
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$disown()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
self, without children
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-transform"></a>}}
\if{latex}{\out{\hypertarget{method-transform}{}}}
\subsection{Method \code{transform()}}{
Create a new node using transformations described in a Transition object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$transform(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{transition object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a *child* object created by applying the transition
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-modify"></a>}}
\if{latex}{\out{\hypertarget{method-modify}{}}}
\subsection{Method \code{modify()}}{
Modify the current node using transformations described in a Transition object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$modify(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{transition object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
self object created by applying the transition
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get"></a>}}
\if{latex}{\out{\hypertarget{method-get}{}}}
\subsection{Method \code{get()}}{
Retrieve an attribute value by symbol (or character string).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$get(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{bare symbol or character string}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
value of x in node attributes
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-has"></a>}}
\if{latex}{\out{\hypertarget{method-has}{}}}
\subsection{Method \code{has()}}{
Check for presence of attribute by symbol (or character string).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$has(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{bare symbol or character string}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
TRUE iff x exists as an attribute
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Node$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
