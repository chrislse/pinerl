-module(index).
-export([test_find_word/2, test_show_all_words/1]).

% given a test file name, list all words in the text file and the lines of its appearance
% in lexicographic order
% Example:
% 	index:test_show_all_words("gettysburg-address.txt").
test_show_all_words(Filename) ->
	{_, Dict} = perform(Filename),
	List = lists:sort(fun({W1, _}, {W2, _}) -> W1 < W2  
		   end, maps:to_list(Dict)),
	lists:map(fun({W, L}) -> {W, format(lists:reverse(L))} end, List).

% try to find word, in certain text file
% Example:
% 	index:test_find_word("we", "dickens-christmas.txt").
test_find_word(Word, Filename) ->
	{_, Dict} = perform(Filename),
	case maps:find(Word, Dict) of
		{ok, Value} -> {Word, format(lists:reverse(Value))};
		error -> {error, "404 Not Found"}
	end.

% Show the value in form of Linelist
% Example:
% 	[{3,5},{7,7},{11,13}] = format([3,4,5,7,11,12,13]).
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
	
% perform the indexing action in given file
perform(Name) ->
	Lines = get_file_contents(Name),
	lists:foldl(fun(Line, {LineNum, DictMap}) ->
				    CleanLine = remove_nonword(Line),
				    {LineNum + 1, index_word_on_line(DictMap, CleanLine, LineNum)}
		    end, {1, #{}}, Lines).

     
% get rid of non-word characters 
remove_nonword(Line) ->
	Pattern = "[\.]|-|,|'|\"|\\(|\\)|:|<|>|[0-9]|\\[|\\]|/|`|;|!|\\\\",
	Options = [global, {return,list}], 
	L = re:replace(Line, Pattern, " ", Options),
	re:replace(L, "\s+", " ", Options).

index_word_on_line(DictMap, CleanLine, LNum) ->
	Words = re:split(CleanLine,"\s",[{return,list}]),

	lists:foldl(fun(Word, Dict) -> put_word_in_dict(Word, Dict, LNum) end,
		    DictMap, Words).

put_word_in_dict(Word, Dict, _) when Word == [] -> Dict;
put_word_in_dict(WordOrig, Dict, LNum) ->
	Word = string:to_lower(WordOrig), %normalize the words
	Value = case maps:find(Word, Dict) of 
			{ok, LineNums} -> if 
						  hd(LineNums) == LNum -> LineNums;
						  true ->  [LNum | LineNums]
					  end;
			error -> [LNum]
		end,
	maps:put(Word, Value, Dict).


% ----------------------------------------------------
% ----------functions from original index.erl--------
% ----------------------------------------------------

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
