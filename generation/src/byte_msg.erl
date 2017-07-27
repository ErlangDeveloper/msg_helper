%%%-------------------------------------------------------------------
%%% @author zhouwenhao
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%此文件为自动生成，请勿修改
%%% @end
%%% Auto Created : 2017-7-27 11:4:39
%%%-------------------------------------------------------------------
-module(byte_msg).
-include("error_code.hrl").
-export([init_nif/0, decode/2, encode/1]).

init_nif()->
	create(),
	init(),
	ok.

create()->
	ets:new(byte_msg_decode,[set,named_table,public,{keypos, 1}]),		%%{id,msgname,mod,decode fun}
	ets:new(byte_msg_encode,[set,named_table,public,{keypos, 2}]),		%%{id,msgname,encode fun}
	[].

init()->
   ets:insert(byte_msg_decode,{10000,test,m_test,decode_test}),
   ets:insert(byte_msg_encode,{10000,test,encode_test}),
   ets:insert(byte_msg_decode,{10001,ping_s2c,m_test,decode_ping_s2c}),
   ets:insert(byte_msg_encode,{10001,ping_s2c,encode_ping_s2c}).

decode(Id, Bin)->
		case ets:lookup(byte_msg_decode, Id) of
			[{_,_,Mod,Fun}] ->
				{Mod,ori_pb:Fun(Bin)};
			_ ->
				{error, ?ERROR_CMD_NOT_EXIST}
		end.

encode(Record)->
		case ets:lookup(byte_msg_encode, element(1, Record)) of
			[{Id,_,Fun}] ->
				Bin = ori_pb:Fun(Record),
				<<Id:32, Bin/binary>>;
			_ ->
				{error, ?ERROR_CMD_NOT_EXIST}
		end.
