/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

    Changes by:    Riccardo Zese
    E-mail:        riccardo.zese@unife.it
*/

:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(trill_on_swish).

/** <module> Load the SWISH server for local usage

This module loads the SWISH server for default local usage, i.e., mostly
for developing SWISH. The file `daemon.pl` can be used as a start to run
it as a server process.

This file is normally used from  `run.pl`,   which  is started like this
from   the   shell   to   start   the   SWISH   server   accessible   on
http://localhost:3050/

    swipl run.pl

@see	run.pl and daemon.pl
*/

%%   server is det.
%%   server(?Port) is det.
%
%    Start the web-server on Port.  Port  may   be  unbound  to make the
%    system  select  a  free  port.  Port  can   also  be  of  the  form
%    `localhost:Port`  to  bind  the  server    only  to  the  localhost
%    interface.

server :-
	%server(localhost:3050).
    server(3050).
server(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]).
