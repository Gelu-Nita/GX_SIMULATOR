#if !defined(MFO_20170228_0207_ASCMAP4IDL_H)
#define MFO_20170228_0207_ASCMAP4IDL_H

#include "idl_export_ext.h"

class CascMap4IDL
{
    // Attributes
public:
    CidlPassParameterMap map;
    int lng;

// Construction
public:
	CascMap4IDL(int Length)
        : lng(0)
    {
        maketerm(0);
    }
	virtual ~CascMap4IDL()
    {
    }

// Operations
protected:
    void makekey(char *key, int pos)
    {
        int keylng = (int)strlen(key);
        map.items[pos].itemName.s = new char[keylng];
        strncpy(map.items[pos].itemName.s, key, keylng);
        map.items[pos].itemName.slen = keylng;
        map.items[pos].itemName.stype = 0;
    }

    void maketerm(int pos)
    {
        makekey(IDLTERMINATIONKEY, pos);
    }

public:
    int insert(char *key, double value)
    {
        // NB! delete old allocated
        makekey(key, lng);
        map.items[lng].itemValue = value;
        lng++;

        maketerm(lng);

        return lng;
    }
    //int find(char *key, mapType& value);
    //int size();
};

#endif // MFO_20170228_0207_ASCMAP4IDL_H