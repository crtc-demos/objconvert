// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__AFD56823_0B43_11D4_8B0F_0050BAC83302__INCLUDED_)
#define AFX_STDAFX_H__AFD56823_0B43_11D4_8B0F_0050BAC83302__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

	typedef int8_t		sbyte;
	typedef uint8_t		ubyte;
	typedef int16_t		sword;
	typedef uint16_t	uword;
	typedef int32_t		sdword;
	typedef uint32_t	udword;
	typedef int64_t		sqword;
	typedef uint64_t	uqword;
	typedef float		sfloat;

	#define	null	NULL
	#define RELEASE(x)		{ if (x != null) delete x;		x = null; }
	#define RELEASEARRAY(x)	{ if (x != null) delete []x;	x = null; }

	inline void ZeroMemory(void* addr, udword size)
	{
		memset(addr, 0, size);
	}

	inline void CopyMemory(void* dest, const void* src, udword size)
	{
		memcpy(dest, src, size);
	}

	inline void FillMemory(void* dest, udword size, ubyte val)
	{
		memset(dest, val, size);
	}

#include "RevisitedRadix.h"
#include "CustomArray.h"
#include "Adjacency.h"
#include "Striper.h"

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__AFD56823_0B43_11D4_8B0F_0050BAC83302__INCLUDED_)
