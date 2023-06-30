/* Shell sort macro */

#define sort(len,i,j)                     \
   { int _i,_is,_p,_m,_len1;              \
     _m=_len1=len; _p=_m/2;               \
     while(_p)                            \
       { _m=((_p>>(_m>15 ? 1 : 2))<<1)+1; \
	 for(_is=1;_is<=_len1-_m;_is++)   \
	   { for(_i=_is;_i>=1;_i-=_m)     \
	       { i=_i-1; j=_i+_m-1;       \
		 if(

#define exch        ) break;

#define endsort                           \
	       }                          \
	   }                              \
	 _p>>=1;                          \
       }                                  \
   }

