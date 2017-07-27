%%%-------------------------------------------------------------------
%%% @author zhouwenhao
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 二月 2016 10:49
%%%-------------------------------------------------------------------
-module(message).
-author("baymaxzhou").

-define(UNICODE2LIST(A), binary_to_list(unicode:characters_to_binary(A))).
-define(Keywords, ["required", "message", "optional", "repeated"]).

-record(msg_original, {name, id, packet, info, type, attrs}). %% 原始消息格式
-record(msg_attr, {name, type, format, info}).		%% 消息内容
-record(class, {name, info, attrs}).

%% API
-export([make/0, choose/3]).

make()->
	{ok, [{msg_path, MsgPath},{class_path, ClassPath},{game_msg_path, GameMsgPath},{proto_msg_path, ProtoMsgPath},{byte_msg_path, ByteMsgPath},
	{import, List}]} = file:consult(lists:concat([?MODULE, ".config"])),
	make({MsgPath, ClassPath, GameMsgPath, ProtoMsgPath, ByteMsgPath, List}),
 	protobuffs_compile:scan_file_src(ProtoMsgPath),
	halt().

make({MsgPath, ClassPath, _GameMsgPath, ProtoMsgPath, ByteMsgPath, ImportList})->
	{ok, File} = file:consult(MsgPath),
	{ProtoBuff, _Json} = lists:splitwith(fun(E)-> E#msg_original.type =:= byte end, File),

	{ok, Files} = file:list_dir(ClassPath),
	Classes = choose(ClassPath, lists:sort(Files), []),
	
	[ProtoName|_] = lists:reverse(string:tokens(ProtoMsgPath, "/")),
	[ProtoFirstName|_] = string:tokens(ProtoName, "."),

%% 	file:delete(GameMsgPath),
%% 	{ok, GameMsgFd} = file:open(GameMsgPath, [write,append]),
%% 	write(game_msg, [GameMsgFd, Json, Classes]),
%% 	file:close(GameMsgFd),

	Import = lists:merge([grouping(ImportProtoBufPath, ImportProtoStruct, Max) || {ImportProtoBufPath, Max, ImportProtoStruct}<-ImportList]),

	file:delete(ProtoMsgPath),
	{ok, ProtoMsgFd} = file:open(ProtoMsgPath, [write,append]),
	write(proto_msg, [ProtoMsgFd, ProtoBuff, Classes, Import]),
	file:close(ProtoMsgFd),

	file:delete(ByteMsgPath),
	{ok, ByteMsgFd} = file:open(ByteMsgPath, [write,append]),
	write(byte_msg, [ProtoFirstName ++ "_pb", ByteMsgFd, ProtoBuff]),
	file:close(ByteMsgFd).

choose(_, [] , List)->
	List;
choose(Path, [File|Tail], List)->
	ItemPath = Path ++ File,
	case filelib:is_dir(ItemPath) of
		true  ->
			choose(Path, Tail, List);
		_ ->
			case lists:reverse(File) of
				"gifnoc" ++ _  ->
					{ok, Class} = file:consult(ItemPath),
					choose(Path, Tail, Class ++ List);
				_ ->
					choose(Path, Tail, List)
			end
	end.

write(game_msg, [Fd, Json, Classes])->
	io:format(Fd,
		"~s~n", [binary_to_list(unicode:characters_to_binary("%% -record(msg_define, {name::消息名, id::消息id, packet::消息处理模块, info::消息描述, attrs::消息结构}). %% 消息格式
%% -record(msg_attr, {name::内容key, type::数据类型, format::数据格式, info::内容描述}).		%% 消息内容
%% -record(info_define, {name::结构名, info::结构描述, attrs::子结构}).		%% 自定义消息格式
%% msg_attr.type  ::  int | string | json | other_msg(自定义object)
%% msg_attr.format  ::  list(数据列表) | normal(默认数据) | kvobject(自定义key,根据value自定组object或objectlist)"))]),
	[io:format(Fd, "~n{info_define, ~w,  ~c~s~c,~n   [~s~n   ]~n }.~n", [Name,  $",?UNICODE2LIST(Info), $",
		string:join([io_lib:format("~n      {msg_attr, ~w, ~w, ~w, ~c~s~c}",[A, case B of int32 -> int; uint32 -> int; sint32 -> int; int64 -> int; uint64 -> int; sint64 -> int; bytes ->string;  _ -> B end,
			case C of null -> normal; Other -> Other end, $", ?UNICODE2LIST(D), $"])
			|| #msg_attr{name = A, type = B, format = C, info = D}<-Attrs], ",")])
		||#class{name = Name, info = Info, attrs = Attrs}<-Classes],
	[io:format(Fd, "~n{msg_define, ~w, ~w, ~w, ~c~s~c,~n   [~s~n   ]~n }.~n", [Name, Id, Pk, $",?UNICODE2LIST(Info), $",
		string:join([io_lib:format("~n      {msg_attr, ~w, ~w, ~w, ~c~s~c}",[A, case B of int32 -> int; uint32 -> int; sint32 -> int; int64 -> int; uint64 -> int; sint64 -> int; bytes ->string;  _ -> B end,
			case C of null -> normal; Other -> Other end, $",?UNICODE2LIST(D), $"])
			|| #msg_attr{name = A, type = B, format = C, info = D}<-Attrs], ",")])
		||#msg_original{name = Name, id = Id, packet = Pk, info = Info, attrs = Attrs}<-Json];
write(proto_msg, [Fd, ProtoBuff, Classes, Import])->
	io:format(Fd, "//Import Structs~n", []),
	[io:format(Fd,"~s ~s{~n~s}~n~n", [M, Name, [io_lib:format("\t~s ~s ~s = ~s;~n", [A, B, C, D])||{A, B, C, D}<-List]])
		||{M, Name, List}<-Import],
	io:format(Fd, "//End Import Structs~n~n", []),
	AlreadyWrite = [list_to_atom(Name)||{_, Name, _}<-Import],
	[io:format(Fd, "//~s~nmessage ~w{    ~s~n}~n~n", [?UNICODE2LIST(Info), Name,
		[io_lib:format("~n  //~s~n      ~w ~w ~w = ~w;",[?UNICODE2LIST(D), case C of normal ->required; list -> repeated; null ->optional; Other -> Other end, B, A, Index]) || {#msg_attr{name = A, type = B, format = C, info = D}, Index}
			<-lists:zip(Attrs, lists:seq(1, length(Attrs)))]])
		||#class{name = Name, info = Info, attrs = Attrs}<-Classes, not lists:member(Name, AlreadyWrite)],
	[io:format(Fd, "//~s~nmessage ~w{    ~s~n}~n~n", [?UNICODE2LIST(Info), Name,
		[io_lib:format("~n  //~s~n      ~w ~w ~w = ~w;",[?UNICODE2LIST(D), case C of normal ->required; list -> repeated; null ->optional; Other -> Other end, B, A, Index]) || {#msg_attr{name = A, type = B, format = C, info = D}, Index}
			<-lists:zip(Attrs, lists:seq(1, length(Attrs)))]])
		|| #msg_original{name = Name, info = Info, attrs = Attrs} <-ProtoBuff, not lists:member(Name, AlreadyWrite)];
write(byte_msg, [ProtoFirstName, Fd, ProtoBuff])->
	io:format(Fd,
"%%%-------------------------------------------------------------------
%%% @author zhouwenhao
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%~s
%%% @end
%%% Auto Created : ~s
%%%-------------------------------------------------------------------
-module(byte_msg).
-include(~c~s~c).
-export([init_nif/0, decode/2, encode/1]).

init_nif()->
	create(),
	init(),
	ok.

create()->
	ets:new(byte_msg_decode,[set,named_table,public,{keypos, 1}]),		%%{id,msgname,mod,decode fun}
	ets:new(byte_msg_encode,[set,named_table,public,{keypos, 2}]),		%%{id,msgname,encode fun}
	[].~n", [binary_to_list(unicode:characters_to_binary("此文件为自动生成，请勿修改")), time_to_local_string(unixtime()), $","error_code.hrl", $"]),
	io:format(Fd, "~ninit()->~n~s.~n",
		[
			io_lib:format(string:join(
				[
						"   ets:insert(byte_msg_decode," ++ "{" ++ integer_to_list(Id) ++ "," ++ atom_to_list(Name) ++ "," ++ atom_to_list(Packet) ++ "," ++ lists:concat(["decode_", Name]) ++ "}),~n"
						++"   ets:insert(byte_msg_encode," ++ "{" ++ integer_to_list(Id) ++ "," ++ atom_to_list(Name) ++ ","  ++ lists:concat(["encode_", Name]) ++ "})"
					||#msg_original{id= Id, name = Name, packet = Packet}<-ProtoBuff
				], ",~n"), [])
		]),
	io:format(Fd, "~ndecode(Id, Bin)->
		case ets:lookup(byte_msg_decode, Id) of
			[{_,_,Mod,Fun}] ->
				{Mod,~s:Fun(Bin)};
			_ ->
				{error, ?ERROR_CMD_NOT_EXIST}
		end.~n", [ProtoFirstName]),
	io:format(Fd, "~nencode(Record)->
		case ets:lookup(byte_msg_encode, element(1, Record)) of
			[{Id,_,Fun}] ->
				Bin = ~s:Fun(Record),
				<<Id:32, Bin/binary>>;
			_ ->
				{error, ?ERROR_CMD_NOT_EXIST}
		end.~n", [ProtoFirstName]).

unixtime() ->
	{MegaSecs, Secs, _MicroSecs} = os:timestamp(),
	MegaSecs * 1000000 + Secs.

time_to_local_string(Time)->
	{{Year, Month , Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(s_to_now(Time)),
	lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]).

s_to_now(S)->
	{S div 1000000, S rem 1000000, 0}.


%%-------------------------------------------
%% 拷贝
%%-------------------------------------------
grouping(Path, ImportStructs, Max)->
	put(words, []),
	foreachWordInString(del_notes(Path, Max), fun(X)->Dict = get(words), put(words,[X|Dict]) end),
	List = lists:reverse(get(words)),
	List1 = g(List, [], []),
	List2= [m(mf(X, []))||X<-List1, X=/= []],
	[X||X = {_, Name, _}<-List2, lists:member(Name, ImportStructs)].

del_notes(Path, Max)->
	{ok, File} = file:open(Path, read),
	Return = l(File, "", 1, Max),
	file:close(File),
	Return.

l(Fd, Words, Index, Max) when Index > Max->
	case io:get_line(Fd, "") of
		eof ->
			Words;
		Line ->
			TempLine =
				case string:str(Line, "import") of
					0 ->
						f($e, Line, "");
					_ ->
						""
				end,
			l(Fd, Words ++ TempLine, Index + 1, Max)
	end;
l(Fd, Words, Index, Max)->
	io:get_line(Fd, ""),
	l(Fd, Words, Index + 1, Max).


f(_, [], Other)->
	lists:reverse(Other);
f($/, [$/|_], Other)->
	lists:reverse(Other);
f(_, [$/|T], Other)->
	f($/, T, Other);
f(_, [H|T], Other)->
	f(H, T, [H|Other]).


g([], List, Record)->
	lists:reverse([lists:reverse(Record)|List]);
g(["message"|Tail], List, Record)->
	g(Tail, [lists:reverse(Record)|List], ["message"]);
g([Other|Tail], List, Record)->
	case string:tokens(Other, ".") of
		[Other] ->
			g(Tail, List, [Other|Record]);
		StrList when is_list(StrList) ->
			g(Tail, List, [lists:last(StrList)|Record])
	end.


mf([], List)->
	lists:reverse(List);
mf(["default", Value|Tail], List)->
	case lists:member(Value, ?Keywords) of
		true ->
			mf(Tail, [Value|List]);
		_ ->
			mf(Tail, List)
	end	;
mf([H|Tail], List)->
	mf(Tail, [H|List]).


m(["message", Name|Tail])->
	list_to_tuple(["message", Name, m(Tail, [])]).

m([], List)->
	lists:reverse(List);
m([A, B, C, D|Tail], List)->
	m(Tail, [{A, B, C, D}|List]).

foreachWordInString(Str, F) ->
	case get_word(Str) of
		no ->
			void;
		{Word, Str1} ->
			F(Word),
			foreachWordInString(Str1, F)
	end.


isWordChar(X) when $A=< X, X=<$Z -> true;
isWordChar(X) when $0=< X, X=<$9 -> true;
isWordChar(X) when $a=< X, X=<$z -> true;
isWordChar(X) when X =:= $_ -> true;
isWordChar(X) when X =:= $. -> true;
isWordChar(_)  -> false.

get_word([H|T]) ->
	case isWordChar(H) of
		true  -> collect_word(T, [H]);
		false -> get_word(T)
	end;
get_word([]) ->
	no.

collect_word([H|T]=All, L) ->
	case isWordChar(H) of
		true  -> collect_word(T, [H|L]);
		false -> {lists:reverse(L), All}
	end;
collect_word([], L) ->
	{lists:reverse(L), []}.