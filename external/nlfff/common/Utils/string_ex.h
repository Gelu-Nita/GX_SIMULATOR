#pragma once

#include <cctype>
#include <string>
#include <cstring>
#include <algorithm>

std::string str_ex_tolower(std::string s);
size_t str_ex_tolower(char *s);
bool str_ex_compare(char *s1, char *s2, bool case_sens = true);
