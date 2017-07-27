-module(ori_pb).

-export([encode/1, encode/2, decode/2,
	 encode_ping_s2c/1, decode_ping_s2c/1, encode_test/1,
	 decode_test/1, encode_crypto_packet/1,
	 decode_crypto_packet/1, encode_a/1, decode_a/1]).

-record(ping_s2c, {id, cp, info}).

-record(test, {state}).

-record(crypto_packet, {record_name, token}).

-record(a, {name, age}).

encode(Record) ->
    encode(erlang:element(1, Record), Record).

encode_ping_s2c(Record)
    when is_record(Record, ping_s2c) ->
    encode(ping_s2c, Record).

encode_test(Record) when is_record(Record, test) ->
    encode(test, Record).

encode_crypto_packet(Record)
    when is_record(Record, crypto_packet) ->
    encode(crypto_packet, Record).

encode_a(Record) when is_record(Record, a) ->
    encode(a, Record).

encode(a, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#a.name, none), string, []),
		      pack(2, required, with_default(_Record#a.age, none),
			   uint32, [])]);
encode(crypto_packet, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#crypto_packet.record_name,
					none),
			   string, []),
		      pack(2, required,
			   with_default(_Record#crypto_packet.token, none),
			   bytes, [])]);
encode(test, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#test.state, none), uint32,
			   [])]);
encode(ping_s2c, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#ping_s2c.id, none), uint32, []),
		      pack(2, required,
			   with_default(_Record#ping_s2c.cp, none),
			   crypto_packet, []),
		      pack(3, required,
			   with_default(_Record#ping_s2c.info, none), a, [])]).

with_default(undefined, none) -> undefined;
with_default(undefined, Default) -> Default;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    RecName = erlang:element(1, Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _) ->
    protobuffs:encode(FNum, Data, Type).

decode_ping_s2c(Bytes) -> decode(ping_s2c, Bytes).

decode_test(Bytes) -> decode(test, Bytes).

decode_crypto_packet(Bytes) ->
    decode(crypto_packet, Bytes).

decode_a(Bytes) -> decode(a, Bytes).

decode(a, Bytes) ->
    Types = [{2, age, uint32, []}, {1, name, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(a, Decoded);
decode(crypto_packet, Bytes) ->
    Types = [{2, token, bytes, []},
	     {1, record_name, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(crypto_packet, Decoded);
decode(test, Bytes) ->
    Types = [{1, state, uint32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(test, Decoded);
decode(ping_s2c, Bytes) ->
    Types = [{3, info, a, [is_record]},
	     {2, cp, crypto_packet, [is_record]},
	     {1, id, uint32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(ping_s2c, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(<<Bytes/binary>>, Types, Acc) ->
    {{FNum, WireType}, Rest} =
	protobuffs:read_field_num_and_wire_type(Bytes),
    case lists:keysearch(FNum, 1, Types) of
      {value, {FNum, Name, Type, Opts}} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {V, R} = protobuffs:decode_value(WireType,
								   bytes, Rest),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  {V, R} = protobuffs:decode_value(WireType,
								   Type, Rest),
				  {unpack_value(V, Type), R}
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([Value1 | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types, [{FNum, Name, [Value1]} | Acc])
		end;
	    false ->
		decode(Rest1, Types, [{FNum, Name, Value1} | Acc])
	  end;
      false -> exit({error, {unexpected_field_index, FNum}})
    end.

unpack_value(<<Binary/binary>>, string) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(a, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, a), Record, Name,
					 Val)
		end,
		#a{}, DecodedTuples);
to_record(crypto_packet, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, crypto_packet),
					 Record, Name, Val)
		end,
		#crypto_packet{}, DecodedTuples);
to_record(test, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, test), Record,
					 Name, Val)
		end,
		#test{}, DecodedTuples);
to_record(ping_s2c, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, ping_s2c), Record,
					 Name, Val)
		end,
		#ping_s2c{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

