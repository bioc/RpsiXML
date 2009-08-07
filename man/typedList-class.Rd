\name{typedList-class}
\Rdversion{1.1}
\docType{class}
\alias{typedList-class}
\alias{typedList-methods}
\alias{typedList}
\alias{typedList,ANY-method}
\alias{show,typedList-method}

\title{TYPE SAVE LIST}
\description{ A list of objects of the same type (class)}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("typedList",
    .Data="list", type="type")}, or the constructor named
  \sQuote{typedList}. See the examples below.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}, should be a list
    of objects belonging to the same class (type)}
    \item{\code{type}:}{Object of class \code{"character"}, the type
      (class) of the object}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{typedList}{\code{signature("ANY")}: create an object of
      \code{typedList}. The method is called in the following form:
      \code{typedList(..., type="character")}. The \sQuote{...} is
      coerced in a list by \code{\link{list}}. If \sQuote{type} is provided, the items of the
      list will be checked to confirm that class (type). If \sQuote{type} is
      missing, the class of the first item will be taken as the
      \sQuote{type} and then the list is checked.}
    \item{show}{\code{signature(object = "typedList")}: print the object}
  }
}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\note{ This class supports multiple classes. In this case the order of the
  'type' must be coherent with the order of object classes. }

%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
## constructor
typedList(hsv="champ", fcb="zwe", type="character")

## type can be omitted
## the class information of the first item in the list is used
typedList(list(a=3L, b=3L))

## multiple classes
tempObj <- "tempObj"
class(tempObj) <- c("class1", "class2")
tempList <- list(o1=tempObj, o2=tempObj)
typedList(tempList)
rm("tempObj", "tempList")
}
\keyword{classes}
