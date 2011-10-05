#include <vector>
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <inttypes.h>

#define __MAX(a,b) (a<b ? b : a)

#if __GNUC__
# if __x86_64__
#  ifndef BN_B
#   define BN_B 1000000000000000000ULL
#   define BN_E 18
#   define BN_T uint64_t
#  endif
# else
# endif
#endif

#ifndef BN_B
# define BN_B 100000000UL
# define BN_E 8
# define BN_T uint32_t
#endif

namespace mandor
{

  template <typename T>
  class bignum_base
  {
  public:
    bignum_base(const std::vector<T> &ds, T base) :
      _digits(ds),
      _base(base)
    {
      if (! std::numeric_limits<T>::is_modulo)
        throw(std::runtime_error("modular arithmetic needed"));
      if (std::numeric_limits<T>::is_signed)
        throw(std::runtime_error("unsigned number needed"));
      if (! std::numeric_limits<T>::is_integer)
        throw(std::runtime_error("integer number required"));
      // make sure addition will not overflow
      if ((_base-1) > (std::numeric_limits<T>::max()-_base))
        throw(std::runtime_error("2base must not exceed T_max [addition]"));
    }

    bignum_base(const bignum_base<T> &a) :
      _digits(a._digits),
      _base(a._base)
    {}

    virtual ~bignum_base() = 0;

    inline
    int size() const
    { return(_digits.size()); }

    inline
    const T &digit(const int &k) const
    { return(_digits[k]); }

    inline
    void plus(bignum_base<T> &r, const bignum_base<T> &o) const
    {
      if (_base != o._base)
        throw(std::runtime_error("numbers must have same base"));

      r._digits.clear();
      r._base = _base;

      int la  = _digits.size();
      int lb  = o._digits.size();
      int l   = __MAX(la, lb);
      T carry = 0;
      for (int k=0; k<l; k++)
      {
        T a = (k<la ? _digits[k]   : 0);
        T b = (k<lb ? o._digits[k] : 0);
        // N.B.: C++ already does arithmetic modulo N, making the
        //       operator % unecessary. However, we might be using a
        //       base /= T_max.
        T d = (a + (b + carry)) % _base;
        r._digits.push_back(d);
        // TODO: use add-with-carry -like instruction
        carry = (d>=a && d>=(b+carry)) ? 0 : 1;
      }
      if (carry)
        r._digits.push_back(carry);
    }

    virtual bignum_base<T> &operator=(const bignum_base<T> &a)
    {
      this->_digits = a._digits;
      this->_base   = a._base;
      return(*this);
    }

  protected:
    inline
    void append(const T &d)
    { _digits.push_back(d); }

  private:
    // LITTLE ENDIAN
    std::vector<T> _digits;
    T _base;
  };

  template <typename T> inline
  bignum_base<T>::~bignum_base()
  {}

  class bignum : public bignum_base<BN_T>
  {
  public:
    bignum() :
      bignum_base<BN_T>(std::vector<BN_T>(), BN_B)
    {}

    bignum(const std::string &s) :
      bignum_base<BN_T>(std::vector<BN_T>(), BN_B)
    { from_string(s); }

    bignum(const bignum &o) :
      bignum_base<BN_T>(o)
    {}

    virtual ~bignum()
    {}

    virtual bignum &operator=(const bignum_base<BN_T> &a)
    {
      static_cast<bignum_base<BN_T>*>(this)->operator=(a);
      return(*this);
    }

    bignum operator+(const bignum &o) const
    {
      bignum r;
      plus(r, o);
      return(r);
    }

    std::string tostring_() const
    {
      std::ostringstream s;
      tostring(s);
      return(s.str());
    }

    // N.B.: This only works when the base is in the form 10^k.
    void tostring(std::ostringstream &s) const
    {
      s.fill('0');
      int l = size();
      for (int k=l; k>0; k-=1)
      {
        if (k==l)
          s.width(0);
        else
          s.width(BN_E);
        s << std::right;
        s << digit(k-1);
      }
    }

  private:
    // N.B.: This only works when the base is in the form 10^k.
    void from_string(const std::string &n)
    {
      for (int k_=n.size(); k_>0; k_-=BN_E)
      {
        int f = __MAX(k_ - BN_E, 0);
        int t = (k_-BN_E<0 ? k_%BN_E : BN_E);
        append(std::strtoull(n.substr(f, t).c_str(), 0, 10));
      }
    }

  };

}
