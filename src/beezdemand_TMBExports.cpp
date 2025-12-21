// beezdemand TMB model dispatcher
// This file registers TMB models for the beezdemand package

#define TMB_LIB_INIT R_init_beezdemand
#include <TMB.hpp>
#include "HurdleDemand2RE.h"
#include "HurdleDemand3RE.h"
#include "HurdleCrossPrice2RE.h"
#include "HurdleCrossPrice3RE.h"
#include "JointHurdleSaturated.h"
#include "JointHurdleLatent.h"

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRING(model);
  if (model == "HurdleDemand2RE") {
    return HurdleDemand2RE(this);
  } else if (model == "HurdleDemand3RE") {
    return HurdleDemand3RE(this);
  } else if (model == "HurdleCrossPrice2RE") {
    return HurdleCrossPrice2RE(this);
  } else if (model == "HurdleCrossPrice3RE") {
    return HurdleCrossPrice3RE(this);
  } else if (model == "JointHurdleSaturated") {
    return JointHurdleSaturated(this);
  } else if (model == "JointHurdleLatent") {
    return JointHurdleLatent(this);
  } else {
    error("Unknown model");
  }
  return Type(0);
}
