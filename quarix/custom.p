
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Adam
    Created     : Wed Feb 12 09:24:42 EET 2014
    Notes       :
  ----------------------------------------------------------------------*/
using com.quarix.data.filter.__.
using com.quarix.data.filter.is.

__:In(
	__:In(
		__:On('age', com.quarix.data.filter.is:GE, '200')
			:And('age', com.quarix.data.filter.is:GT, '100')
			:And('age', com.quarix.data.filter.is:EQ, '300')
		)
		:and('color', com.quarix.data.filter.is:EQ, 'green')
	):Or('street', com.quarix.data.filter.is:EQ, 'aaa')
	:OR(__:On('street', com.quarix.data.filter.is:LT, 'a')).
