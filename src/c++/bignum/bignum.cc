#include <vector>
#include <sstream>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <inttypes.h>

#define MAX(a,b) (a<b ? b : a)

template <typename T>
class bignum
{
public:
  bignum(const std::vector<T> &ds) :
    _digits(ds),
    _base(std::numeric_limits<T>::max())
  {
    if (! std::numeric_limits<T>::is_modulo)
      throw(std::runtime_error("modular arithmetic needed"));
    if (std::numeric_limits<T>::is_signed)
      throw(std::runtime_error("unsigned number needed"));
    if (! std::numeric_limits<T>::is_integer)
      throw(std::runtime_error("integer number required"));
  }

  bignum(const bignum<T> &a) :
    _digits(a._digits),
    _base(a._base)
  {}

  ~bignum()
  {}

  bignum<T> operator+(const bignum<T> &o) const
  {
    std::vector<T> cdigits;
    int k;
    int la  = _digits.size();
    int lb  = o._digits.size();
    int l   = MAX(la, lb);
    T carry = 0;
    for (k=0; k<l; k++)
    {
      T a = (k<la ? _digits[k]   : 0);
      T b = (k<lb ? o._digits[k] : 0);
      T d = a + b + carry;
      cdigits.push_back(d);
      carry = (d>=a && d>=b) ? 0 : 1;
    }
    if (carry)
      cdigits.push_back(carry);
    return(bignum<T>(cdigits));
  }

  bignum<T> &operator=(const bignum<T> &a)
  {
    this->_digits = a._digits;
    this->_base   = a._base;
    return(*this);
  }

private:
  std::vector<T> _digits;
  T _base;
};
