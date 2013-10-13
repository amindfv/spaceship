-module(hanoi_tower_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% Tests that the moves all result in the correct actions:
solve_test() ->
   T0 = {tower, _} = hanoi_tower:new_tower(3),
   {legal, T1 = {tower, _}} = hanoi_tower:move(T0, first, last),
   {legal, T2 = {tower, _}} = hanoi_tower:move(T1, first, middle),
   {legal, T3 = {tower, _}} = hanoi_tower:move(T2, last, middle),
   {legal, T4 = {tower, _}} = hanoi_tower:move(T3, first, last),
   {legal, T5 = {tower, _}} = hanoi_tower:move(T4, middle, first),
   {legal, T6 = {tower, _}} = hanoi_tower:move(T5, middle, last),
   solved                   = hanoi_tower:move(T6, first, last).

cant_move_from_empty_test() ->
   T = hanoi_tower:new_tower(3),
   illegal = hanoi_tower:move(T, last, first).

cant_move_larger_test() ->
   T0 = hanoi_tower:new_tower(3),
   {legal, T1} = hanoi_tower:move(T0, first, last),
   illegal     = hanoi_tower:move(T1, first, last).

position_size_test() ->
   T0 = hanoi_tower:new_tower(3),
   {legal, T1} = hanoi_tower:move(T0, first, last),
   {legal, T2} = hanoi_tower:move(T1, first, middle),
   {legal, T3} = hanoi_tower:move(T2, last, middle),
   2 = hanoi_tower:position_size(T3, middle).

main(_) ->
   eunit:test(?MODULE, [verbose]).
