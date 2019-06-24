:- use_module(lambda).
:- use_module(library(yall)).


% These are examples of how lambda can be used

% Confirms that all elements are x
% Can also repeat x into a list with fixed size
all_are_x(X,List):-
    make_lambda(A:- A=X,Func),
    maplist(Func,List),
    flush_lambda.
     % The last step is just good practice
     % It deletes all active lambda clauses

% Multiplies every element with 2
% Normaly you would have had to make a list of the same size
% with all elements being 2 to do something similar with maplist
% +List -OList
double_all(List,OList):-
    make_lambda([A,B]:-B is A * 2,Func),
    maplist(Func,List,OList).

% Gets some random numbers
manual_n_random_numbers(N,Numbers):-
    make_lambda(R:-random(R),Func),
    length(Numbers,N),
    maplist(Func,Numbers).

% Adds some new stuff to the random numbers predicate
% +N, -Nums, +From, +To
manual_n_random(N,Nums,From,To):-
    Diff is To-From,
    manual_n_random_numbers(N,Numbers),
    make_lambda([Rin,Rout]:-(M is Rin*Diff, Rout is M+From),Func),
    maplist(Func,Numbers,Nums).

% generates a N x M matrix of random numbers
manual_random_matrix(Matrix,N,M):-
    length(Matrix,N),
    make_lambda(R:-manual_n_random(N,R,0,M),Func),
    maplist(Func,Matrix).

% Does some rowwise computation on the matrix
manual_row_normalized_matrix(Matrix,Normal):-
    make_lambda(
        [Row,ORow]:-
        (
            sum_list(Row,Sum),
            length(Row,Rowlen),
            Average is Sum/Rowlen,
            make_lambda(
                [Cin,Cout]:-
                (
                    Diff is Cin -Average,
                    Cout is Diff*Diff
                )   
            ,DiffAndSquare),
            maplist(DiffAndSquare,Row,DiffSquares),
            sum_list(DiffSquares,DiffSquare),
            DiffSquareAV is DiffSquare / Rowlen,
            sqrt(DiffSquareAV,SD),
            make_lambda(
                [Value,Norm]:-
                (
                    G is Value - Average,
                    Norm is G/ SD    
                )
            ,NormFunc),
            maplist(NormFunc,Row,ORow)
        )
    ,Func),
    maplist(Func,Matrix,Normal).

auto_n_random_numbers(N,Numbers):-
    length(Numbers,N),
    lambda_map(R:-random(R),Numbers).

auto_n_random(N,Nums,From,To):-
    Diff is To-From,
    auto_n_random_numbers(N,Numbers),
    lambda_map([Rin,Rout]:-(M is Rin*Diff, Rout is M+From),Numbers,Nums).

yall_n_random_numbers(N,Numbers):-
    length(Numbers,N),
    maplist([R]>>random(R),Numbers).

yall_n_random(N,Nums,From,To):-
    Diff is To-From,
    yall_n_random_numbers(N,Numbers),
    maplist({Diff,From}/[Rin,Rout]>>(M is Rin*Diff,Rout is M+From),Numbers,Nums).

auto_random_matrix(Matrix,N,M):-
    length(Matrix,N),
    lambda_map(R:-auto_n_random(N,R,0,M),Matrix).

yall_random_matrix(Matrix,N,M):-
    length(Matrix,N),
    maplist({N,M}/[R]>>yall_n_random(N,R,0,M),Matrix).

auto_row_normalized_matrix(Matrix,Normal):-
    lambda_map(
        [Row,ORow]:-
        (
            sum_list(Row,Sum),
            length(Row,Rowlen),
            Average is Sum/Rowlen,
            lambda_map(
                [Cin,Cout]:-
                (
                    Diff is Cin -Average,
                    Cout is Diff*Diff
                )
            ,Row,DiffSquares),
            sum_list(DiffSquares,DiffSquare),
            DiffSquareAV is DiffSquare / Rowlen,
            sqrt(DiffSquareAV,SD),
            lambda_map(
                [Value,Norm]:-
                (
                    G is Value - Average,
                    Norm is G/ SD    
                )
            ,Row,ORow)
        )
    ,Matrix,Normal).

yall_normalization(Matrix,Normal):-
    maplist(
            [Row,ORow]>>
            (
                sum_list(Row,Sum),
                length(Row,Rowlen),
                Average is Sum/Rowlen,
                maplist(
                    [Cin,Cout]>>
                    (
                        Diff is Cin -Average,
                        Cout is Diff*Diff
                    )
                ,Row,DiffSquares),
                sum_list(DiffSquares,DiffSquare),
                DiffSquareAV is DiffSquare / Rowlen,
                sqrt(DiffSquareAV,SD),
                maplist(
                    [Value,Norm]>>
                    (
                        G is Value - Average,
                        Norm is G/ SD    
                    )
                ,Row,ORow)
        ),Matrix,Normal).

/*
Some example calls:

    all_are_x([_,_,_],5).
    double_all([5,21,4,2,3,1,0,0,0],Result).
    n_random(5,Nums,0,5).

when you are finished:

    flush_lambda_log.

that might make it more clear why one would want to clear 
the created expressions

*/

% A example for the difference of using lambda_map, or exporting the lambda predicate,
% and using it manually:

benchmark(Version,Trials,AV):-
    length(Results,Trials),
    maplist(Version,Results),
    sum_list(Results,Sum),
    AV is Sum/Trials.

yallprocess(TimeTaken):-
    get_time(T1),
    yall_random_matrix(MMat,100,500),
    yall_normalization(MMat,_),
    get_time(T2),
    TimeTaken is T2 - T1,write(finished).

manualprocess(TimeTaken):-
    get_time(T1),
    manual_random_matrix(MMat,100,500),    
    flush_lambda,
    manual_row_normalized_matrix(MMat,_),
    flush_lambda,
    get_time(T3),
    TimeTaken is T3 - T1,write("finish"),nl.

autoprocess(TimeTaken):-
    get_time(T1),
    auto_random_matrix(MMat,100,500),  
    auto_row_normalized_matrix(MMat,_),
    get_time(T3),
    TimeTaken is T3 - T1,write("finish"),nl.

/*
benchmark(autoprocess,20,AV).           AV = 0.06854861974716187
benchmark(manualprocess,20,AV).         AV = 0.16591542959213257
benchmark(yallprocess,20,AV).           AV = 0.22476001977920532

benchmark(autoprocess,100,AV).          AV = 0.07036026239395142
benchmark(manualprocess,100,AV).        AV = 0.19594071865081786
benchmark(yallprocess,200,AV).          AV = 0.24549479842185973

As it would seem, this implementation is faster then yall on average,
when used correctly.
As should be visible I did not test the runtime given the >>> notation.
This is not a mistake, I simply know that it is much much worse, since it 
would need to assert and erase a predicate with every single call

*/