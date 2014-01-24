\name{fnRegSSM}
\alias{fnRegSSM}
\title{Create a State Space Model Representation of Linear Regression Model}
\usage{
  fnRegSSM(y, X, H = NULL, Q = NULL, u = NULL,
    distribution = c("Gaussian", "Poisson", "Binomial"),
    transform = c("none", "ldl", "augment"),
    tolF = .Machine$double.eps^0.5,
    tol0 = .Machine$double.eps^0.5)
}
\arguments{
  \item{X}{A \eqn{n \times k}{n*k} matrix of explanatory
  variables, with each column containing one explanatory
  variable, or a list of length \eqn{p} containing \eqn{X}
  matrices for each series. If X is matrix, it is assumed
  that all \eqn{p} series use same explanatory variables.}

  \item{H}{A \eqn{p \times p}{p*p} covariance matrix (or
  \eqn{p \times p \times n}{p*p*n} array in of time-varying
  case) of the disturbance terms
  \eqn{\epsilon_t}{epsilon[t]} of the observation equation.
  Default gives \eqn{p \times p}{p*p} zero matrix.  Omitted
  in case of non-Gaussian distributions. Augment the state
  vector if you want to add additional noise.}

  \item{Q}{A \eqn{r \times r}{r*r} (or \eqn{r \times r
  \times n}{r*r*n} array in of time-varying case)
  covariance matrix of the disturbance terms
  \eqn{\eta_t}{\eta[t]} of the system equation.  Default is
  \eqn{m \times m}{m*m} zero matrix ie. ordinary
  time-invariant regression.}
}
\description{
  Function regSSM creates a state space representation of
  linear regression model.
}
\details{
  The linear Gaussian state space model is given by

  \deqn{y_t = X_t \beta_t + \epsilon_t,}{y[t] =
  Z[t]\alpha[t] + \epsilon[t], (observation equation)}

  \deqn{\alpha_{t+1} = T_t \alpha_t + R_t
  \eta_t,}{\alpha[t+1] = T[t]\alpha[t] + R[t]\eta[t],
  (transition equation)}

  where \eqn{\epsilon_t ~ N(0,H_t)}{\epsilon[t] ~
  N(0,H[t])}, \eqn{\eta_t ~ N(0,Q_t)}{\eta[t] ~ N(0,Q[t])}
  and \eqn{\alpha_1 ~ N(a_1,P_1)}{\alpha[1] ~ N(a[1],P[1])}
  independently of each other. In case of non-Gaussian
  observations, the observation equation is of form
  \eqn{p(y_t|\theta_t) =
  p(y_t|Z_t\alpha_t)}{p(y[t]|\theta[t]) =
  p(y[t]|Z[t]\alpha[t])}, with
  \eqn{p(y_t|\theta_t)}{p(y[t]|\theta[t])} being one of the
  following:
}
\seealso{
  \code{\link{arimaSSM}} for state space representation of
  ARIMA model, \code{\link{structSSM}} for structural time
  series model, and \code{\link{SSModel}} for custom
  \code{SSModel} object.
}

