%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
{application, runtime_tools,
   [{description,  "RUNTIME_TOOLS"},
    {vsn,          "2.1"},
    {modules,      [appmon_info, dbg,observer_backend,runtime_tools,
                    runtime_tools_sup,erts_alloc_config,
		    ttb_autostart,dyntrace,system_information,
                    scheduler, instrument,
                    msacc]},
    {registered,   [runtime_tools_sup]},
    {applications, [kernel, stdlib]},
    {env,          []},
    {mod,          {runtime_tools, []}},
    {runtime_dependencies, ["stdlib-6.0","mnesia-4.12","kernel-10.0",
			    "erts-15.0"]}]}.
