#ifndef LOG_H_
#define LOG_H_

#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cerrno>
#include <cstring>
#include <unistd.h>

namespace Log {
  class Base {
   public:
    Base(std::ostream& out, const std::string& ansi, const std::string& error,
         const std::string& func)
        : _stream(out) {
      _message << "[" << getpid() << "]: ";
#ifdef ANSI
      _message << "\033[" << ansi << 'm' << error << "\033[m: \033[1m" << func;
      _message << "\033[m: ";
#else
      _message << error << ": " << func << ": ";
#endif
    }

    Base& perror(const std::string& msg) {
      _message << msg << ": " << strerror(errno);
      return *this;
    }

    ~Base() {
      _message << '\n';
      _stream << _message.str() << std::flush;
    }

    template <class T> Base& operator<<(const T& t) {
      _message << t;
      return *this;
    }

   private:
    std::stringstream _message;
    std::ostream &_stream;
  };

  class FatalError : public Base {
    public:
     explicit FatalError(const std::string& func)
       : Base(std::cerr, "1;31", "Fatal error", func) {
     }

     ~FatalError() {
       this->Base::~Base();
       std::abort();
     }
  };

# define ERROR_GENERATOR(stream, type, ansi) \
  class type : public Base { \
   public: \
    explicit type(const std::string& func) \
      : Base(stream, ansi, #type, func) { \
    } \
  };

  ERROR_GENERATOR(std::cerr, Error, "1;31")
  ERROR_GENERATOR(std::cout, Warning, "1;33")
  ERROR_GENERATOR(std::cout, Info, "33")
  ERROR_GENERATOR(std::cout, Debug, "1;32")
  ERROR_GENERATOR(std::cout, Trace, "32")

# undef ERROR_GENERATOR
};

#ifdef __GNUC__
# define FATAL() Log::FatalError(__PRETTY_FUNCTION__)
# define ERROR() Log::Error(__PRETTY_FUNCTION__)
# define WARNING() Log::Warning(__PRETTY_FUNCTION__)
# define INFO() Log::Info(__PRETTY_FUNCTION__)
# define DEBUG() Log::Debug(__PRETTY_FUNCTION__)
# define TRACE() Log::Trace(__PRETTY_FUNCTION__)
#else
# define FATAL() Log::FatalError(__func__)
# define ERROR() Log::Error(__func__)
# define WARNING() Log::Warning(__func__)
# define INFO() Log::Info(__func__)
# define DEBUG() Log::Debug(__func__)
# define TRACE() Log::Trace(__func__)
#endif
#endif  // LOG_H_
