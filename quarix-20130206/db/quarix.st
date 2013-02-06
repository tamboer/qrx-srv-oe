# Quarix database structure file.
#
# CVS					: $Revision: 1.3 $
#						    $Log: quarix.st,v $
#						    Revision 1.3  2008/08/11 06:58:48  marian
#						    imo integration - alpha
#						
#						    Revision 1.3  2007/10/23 07:42:50  marian
#						    runtime area, repository sequence
#						
#						    Revision 1.2  2006/12/13 09:38:56  marian
#						    Tables distribution in repository/session areas.
#						
#						    Revision 1.1  2006/12/11 10:23:47  marian
#						    First database structure release
#						
#
b .
#
d "Schema Area" .
#
# Session database area, stores run-time session data
#
d "session area":7,32 .
#
# Repository database area, stores design time repository
#
d "repository area":8,32 .
#
# Runtime database area, stores runtime application repository
#
d "runtime area":9,32 .

