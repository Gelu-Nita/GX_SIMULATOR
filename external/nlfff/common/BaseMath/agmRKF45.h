#pragma once

class CagmRKF45Vect
{
public:
    int n;
    double *e;

public:
    CagmRKF45Vect(int _n)
        : n(_n),
          e(nullptr) 
    {
        if (n > 0)
        {
            e = new double[n];
            for (int i = 0; i < n; i++)
                e[i] = 0;
        }
    }

    CagmRKF45Vect(int _n, double *v)
        : n(_n),
        e(nullptr)
    {
        if (n > 0)
        {
            e = new double[n];
            for (int i = 0; i < n; i++)
                e[i] = v[i];
        }
    }

    CagmRKF45Vect(const CagmRKF45Vect& v)
        : e(nullptr) 
    {
        n = v.GetLength();
        if (n > 0)
            e = new double[n];
        for (int i = 0; i < n; i++)
            e[i] = v[i];
    }

    void clear()
    {
        delete [] e;
    }

    virtual ~CagmRKF45Vect()
    {
        clear();
    }

    CagmRKF45Vect& operator=(const CagmRKF45Vect& v)
    {
        delete [] e;
        n = v.GetLength();
        if (n > 0)
            e = new double[n];
        for (int i = 0; i < n; i++)
            e[i] = v[i];

        return *this;
    }

    CagmRKF45Vect& operator=(double *v)
    {
        for (int i = 0; i < n; i++)
            e[i] = v[i];

        return *this;
    }

    CagmRKF45Vect& operator+=(const CagmRKF45Vect& v)
    {
        for (int i = 0; i < n; i++)
            e[i] += v[i];

        return *this;
    }

    CagmRKF45Vect& operator*=(const double d)
    {
        for (int i = 0; i < n; i++)
            e[i] *= d;

        return *this;
    }

    int GetLength() const { return n; }

    double& operator[](const int i) const
    {
        // exceptions?
        return e[i];
    }

    const double* v() // not safe!
    {
        return e;
    }
};

typedef uint32_t (*RKF45_FUNCTION_VECTOR)(void *, const double, const CagmRKF45Vect&, CagmRKF45Vect&);
typedef uint32_t (*RKF45_FUNCTION_SCALAR)(void *, const double , const double, double&);
typedef bool  (*RKF45_FUNCTION_VECTOR_COND)(void *, const CagmRKF45Vect&);
typedef bool  (*RKF45_FUNCTION_SCALAR_COND)(void *, const double);

class CagmRKF45
{
public:
    typedef enum {None = 0, EndByStep = 1, End = 2, EndByCond = 3, EndNoMove = 4, 
                  TooManyCalcs = 11, WrongErrBound = 12, TooLittleStep = 13, TooManyExits = 14, WrongCall = 15}  Status;

protected:
    double m_absErr, m_relErr, m_absBoundAchieve;
    Status m_status;
    int m_n;
    double m_t, m_tOut, m_dt;
    bool m_bByStep;

    RKF45_FUNCTION_VECTOR m_funcv;
    CagmRKF45Vect m_vY;
    CagmRKF45Vect m_vYP;
    CagmRKF45Vect m_f1, m_f2, m_f3, m_f4, m_f5;
    RKF45_FUNCTION_VECTOR_COND m_fcondv;

    RKF45_FUNCTION_SCALAR m_funcs;
    double m_dY;
    double m_dYP;
    double m_df1, m_df2, m_df3, m_df4, m_df5;
    RKF45_FUNCTION_SCALAR_COND m_fconds;

    void *m_par;

    double m_h;
    double m_eps, m_u26;
    double m_estCond;
    double m_ae;

    double m_saveRE, m_saveAE;
    int m_nfe, m_kop;
    bool m_bInit;

    bool m_bVect;

// Construction
public:
    CagmRKF45(double absErr, double relErr, RKF45_FUNCTION_VECTOR func, int n, void *par, 
        RKF45_FUNCTION_VECTOR_COND fcond = nullptr, double absBoundAchieve = 0);
    CagmRKF45(double absErr, double relErr, RKF45_FUNCTION_SCALAR func, void *par, 
        RKF45_FUNCTION_SCALAR_COND fcond = nullptr, double absBoundAchieve = 0);
    virtual ~CagmRKF45();

    CagmRKF45::Status calculate(double& t, CagmRKF45Vect& vY, double tOut, bool bByStep);
    CagmRKF45::Status GetStatus() { return m_status; }
    void reinit(double absErr, double relErr, void *par);
    void reinit(void *par);

private:
    void reset(double absErr, double relErr);
    void setAbsErr(double absErr);
    void setRelErr(double relErr);
    CagmRKF45::Status integrator();
    uint32_t fehl(CagmRKF45Vect& s);
    uint32_t fehl(double& s);
    double estCond(uint32_t dwRes);
    double getEeEt(CagmRKF45::Status& status);
};
