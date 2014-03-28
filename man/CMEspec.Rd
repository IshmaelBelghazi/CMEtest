\name{CMEspec}
\alias{CMEspec}
\title{General Interface for covariance matrix specification}
\usage{
CMEspec(smooth = "None", smoothCtrl = list(), estim = "auto",
  estimCtrl = list(), shrink = "None", shrinkCtrl = list(),
  filter = "None", filterCtrl = list(), ...)
}
\arguments{
  \item{smooth}{character specifying to smoothing method.}

  \item{smoothCtrl}{list containing smoothing parameters}

  \item{estim}{character specifying which estimation
  methods ('mle', 'mcd', 'mve,...)}

  \item{estimCtrl}{list containing estimation parameters}

  \item{shrink}{character specifying which covariance
  shrinkage methode to choose}

  \item{shrinkCtrl}{list containing shrinkage parameters}

  \item{filter}{character specifying which filtering method
  to choose}

  \item{filterCtrl}{list containing filtering parameters}
}
\value{
CMEspec: S3 object containing complete specification
}
\description{
Covariance specification object Constructor. .. content for
\description{} (no empty lines) .. This function allows
specification of a covariance objects. .. content for
\details{} ..
}
\author{
Mohamed Ishmael Diwan Belghazi
}

