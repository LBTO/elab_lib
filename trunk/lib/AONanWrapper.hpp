
#ifndef AONANWRAPPER_H
#define AONANWRAPPER_H

#include <istream>
#include <ostream>
#include <iostream>

#include <math.h>

#ifdef TEXT_SERIALIZATION
#include <boost/archive/text_oarchive.hpp>
#else
#include <boost/archive/binary_oarchive.hpp>
#endif

#include "NaNutils.h"

// Bitmask

#define WRAP_NAN    0x01
#define WRAP_INF    0x02
#define WRAP_NEGINF 0x04


template<typename T>
class AONanWrapper
{
  public:
    AONanWrapper( T& value) : _valueref(value) { setFlags(); }
 
    void setFlags() { 
      flags=0;
      if (isnan(_valueref))     flags |= WRAP_NAN;
      if (isinf(_valueref)==1)  flags |= WRAP_INF;
      if (isinf(_valueref)==-1) flags |= WRAP_NEGINF;
    }

    void recalcFromFlags() {
      if (flags & WRAP_NAN)    _valueref = Arcetri::NaNutils::dNaN();
      if (flags & WRAP_INF)    _valueref = Arcetri::NaNutils::dInf();
      if (flags & WRAP_NEGINF) _valueref = -Arcetri::NaNutils::dInf();
    }

    private:
      friend class boost::serialization::access;
      template<class Archive>
      void  serialize(Archive & ar, const unsigned int /* file_version */){

// If serializing in TEXT, use the flags to detect NaNs, etc

#ifdef TEXT_SERIALIZATION
         ar & flags;

         if (flags==0)
            ar & _valueref;
         
         recalcFromFlags();

// Otherwise, just serialize the native type.
// Use a local variable not the reference to keep Boost::serialization happy.

#else
         T value = _valueref;
         ar & value;
         _valueref = value;
#endif

     }

  private:
    T& _valueref;
    int flags;

};

//#include <boost/serialization/tracking.hpp>
//BOOST_CLASS_TRACKING(AONanWrapper<T>, boost::serialization::track_never);


#endif // AONANWRAPPER_H
