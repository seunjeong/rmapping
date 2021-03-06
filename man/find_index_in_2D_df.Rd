% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{find_index_in_2D_df}
\alias{find_index_in_2D_df}
\title{Find index from a 2-D data frame (e.g., map)}
\usage{
find_index_in_2D_df(
  data_2D,
  query,
  num_index_per_query = 1,
  x = "lon",
  y = "lat",
  x_query = "lon",
  y_query = "lat"
)
}
\arguments{
\item{data_2D}{2-D data frame where x/y or lon/lat are available and from which the index is derived.}

\item{query}{Data frame for query. Each row of the query (2-d matrix or 2-column data frame) looks for the closest value in distance (to the row) from the data and return its index corresponding to that value in data_2D.}

\item{num_index_per_query}{the number of indices to be returned - typically just one.}

\item{x}{the x (axis) name in data_2D}

\item{y}{the y (axis) name in data_2D}

\item{x_query}{the x (axis) name in the query data frame}

\item{y_query}{the y (axis) name in the query data frame}
}
\value{

}
\description{
Find index from a 2-D data frame (e.g., map)
}
