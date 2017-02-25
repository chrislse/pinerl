-module(index).
-export([perform/1, test/1, format/1]).

test(Word) ->
	{_, Dict} = perform("dickens-christmas.txt"),
	case maps:find(Word, Dict) of
		{ok, Value} -> {Word, format(lists:reverse(Value))};
		error -> {error, "404 Not Found"}
	end.

format([H | T]) ->
	format([], H, H, T).

format(S, L, H, []) ->
	S ++ [{L, H}];
format(S, L, H, [HT|TT]) ->
	if
		H == HT - 1 ->
			%io:format("The value is: ~p.", [S]),
			format(S, L, HT,  TT);
		true -> % io:format("The true value is ~p", [S]),
		 	format(S ++ [{L, H}], HT, HT, TT) 
	end.
	


perform(Name) ->
	Lines = get_file_contents(Name),
	lists:foldl(fun(Line, {LineNum, DictMap}) ->
				    CleanLine = remove_nonword(Line),
				    {LineNum + 1, index_word_on_line(DictMap, CleanLine, LineNum)}
		    end, {0, #{}}, Lines).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
	{ok,File} = file:open(Name,[read]),
	Rev = get_all_lines(File,[]),
	lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
	case io:get_line(File,"") of
		eof -> file:close(File),
		       Partial;
		Line -> {Strip,_} = lists:split(length(Line)-1,Line),
			get_all_lines(File,[Strip|Partial])
	end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
	io:format("~s~n",[L]),
	show_file_contents(Ls);
show_file_contents([]) ->
	ok.    
     
% get rid of non-word characters 
remove_nonword(Line) ->
        Pattern = "[\.]|-|,|'|\"",
	Options = [global, {return,list}], 
	L = re:replace(Line, Pattern, "", Options),
	re:replace(L, "\s+", " ", Options).

index_word_on_line(DictMap, CleanLine, LNum) ->
	Words = re:split(CleanLine,"\s",[{return,list}]),

	lists:foldl(fun(Word, Dict) ->
				    Value = case maps:find(Word, Dict) of 
						    {ok, LineNums} -> if 
									hd(LineNums) == LNum -> LineNums;
									true ->  [LNum | LineNums]
								       end;
						    error -> [LNum]
					    end,
				    maps:put(Word, Value, Dict)
		    end, DictMap, Words).


