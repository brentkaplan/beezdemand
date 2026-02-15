// beezdemand TMB model dispatcher
// This file registers TMB models for the beezdemand package

#define TMB_LIB_INIT R_init_beezdemand
#include <TMB.hpp>
#include "HurdleDemand2RE.h"
#include "HurdleDemand3RE.h"
#include "HurdleDemand2RE_StdQ0.h"
#include "HurdleDemand3RE_StdQ0.h"
#include "HurdleDemand2RE_SND.h"
#include "HurdleDemand3RE_SND.h"

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRING(model);
  if (model == "HurdleDemand2RE") {
    return HurdleDemand2RE(this);
  } else if (model == "HurdleDemand3RE") {
    return HurdleDemand3RE(this);
  } else if (model == "HurdleDemand2RE_StdQ0") {
    return HurdleDemand2RE_StdQ0(this);
  } else if (model == "HurdleDemand3RE_StdQ0") {
    return HurdleDemand3RE_StdQ0(this);
  } else if (model == "HurdleDemand2RE_SND") {
    return HurdleDemand2RE_SND(this);
  } else if (model == "HurdleDemand3RE_SND") {
    return HurdleDemand3RE_SND(this);
  } else {
    error("Unknown model");
  }
  return Type(0);
}
