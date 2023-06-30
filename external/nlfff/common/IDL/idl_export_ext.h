#pragma once

#include "stdDefinitions.h"
#include <stdio.h>

// remove register specifier
#define IDL_DEBUGGING 2
#include "idl_export.h"

#include <map>
#include <string>
#include "string_ex.h"

#define MFOMAPBUFLEN 256
#define MFOMAPMAXITEMS 256
#define IDLTERMINATIONKEY "!____idl_map_terminator_key___!"

class CidlPassParameterItem
{
public:
    IDL_STRING itemName;
    double itemValue;

    // -- processing --
    typedef enum {Integer = 0, UInteger64 = 1, Double = 2, None = 3, NotFound = 4, End = 5
    }  Status;

    Status parse(std::map<std::string, int> *m_mapInt, std::map<std::string, uint64_t> *m_mapuint64_t, std::map<std::string, double> *m_mapDouble)
    {
        char tItemName[MFOMAPBUFLEN];

        int lng = itemName.slen;
        if (lng <= MFOMAPBUFLEN-1) // parameter name is not too long
        {
            memset(tItemName, 0, MFOMAPBUFLEN);
            strncpy(tItemName, itemName.s, lng);
            str_ex_tolower(tItemName);

            if (!strcmp(tItemName, IDLTERMINATIONKEY))
                return Status::End;

            if (m_mapInt)
            {
                auto search_int = m_mapInt->find(tItemName);
                if (search_int != m_mapInt->end())
                {
                    m_mapInt->at(tItemName) = (int)itemValue;
                    return Status::Integer;
                }
            }

            if (m_mapDouble)
            {
                auto search_double = m_mapDouble->find(tItemName);
                if (search_double != m_mapDouble->end())
                {
                    m_mapDouble->at(tItemName) = (double)itemValue;
                    return Status::Double;
                }
            }

            if (m_mapuint64_t)
            {
                auto search_64 = m_mapuint64_t->find(tItemName);
                if (search_64 != m_mapuint64_t->end())
                {
                    m_mapuint64_t->at(tItemName) = (uint64_t)itemValue;
                    return Status::UInteger64;
                }
            }

            return Status::NotFound;
        }
        else
            return Status::NotFound;
    }
};

class CidlPassParameterMap
{
public:
    CidlPassParameterItem items[MFOMAPMAXITEMS];

    int parse(std::map<std::string, int> *m_mapInt, std::map<std::string, uint64_t> *m_mapuint64_t, std::map<std::string, double> *m_mapDouble)
    {
        int rc = 0;
        
        CidlPassParameterItem *mt = items;
        while (1)
        {
            CidlPassParameterItem::Status s = mt->parse(m_mapInt, m_mapuint64_t, m_mapDouble);
            if (s == CidlPassParameterItem::Status::End)
                break;

            if (s == CidlPassParameterItem::Status::NotFound)
                rc++;

            mt++;
        }

        return rc;
    }
};
