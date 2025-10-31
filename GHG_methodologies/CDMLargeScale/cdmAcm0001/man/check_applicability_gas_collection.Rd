% Generated manually
\name{check_applicability_gas_collection}
\alias{check_applicability_gas_collection}
\title{Check ACM0001 gas collection system readiness}
\usage{check_applicability_gas_collection(collection_system_installed, expected_capture_efficiency, minimum_efficiency = 0.2)}
\arguments{
  \item{collection_system_installed}{Logical flag indicating whether a gas collection system is installed or planned.}
  \item{expected_capture_efficiency}{Expected methane capture efficiency as a fraction.}
  \item{minimum_efficiency}{Minimum acceptable capture efficiency.}
}
\value{Logical value indicating whether the collection system satisfies ACM0001 expectations.}
\description{Validates that the landfill has or will install a gas collection system with adequate capture efficiency to justify ACM0001 application.}
