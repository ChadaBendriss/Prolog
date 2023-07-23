read_xyz_file(File, Points) :-
open(File, read, Stream),
 read_xyz_points(Stream,Points),
 close(Stream).
read_xyz_points(Stream, []) :-
 at_end_of_stream(Stream).
read_xyz_points(Stream, [Point|Points]) :-
 \+ at_end_of_stream(Stream),
 read_line_to_string(Stream,L), split_string(L, "\t", "\s\t\n",
XYZ), convert_to_float(XYZ,Point),
 read_xyz_points(Stream, Points).
convert_to_float([],[]).
convert_to_float([H|T],[HH|TT]) :-
 atom_number(H, HH),
 convert_to_float(T,TT).

random3points(Points, Point3):- length(Points,3),random_permutation(Points,Shuffled),Point3=[X,Y,Z],select(X,Shuffled,Rest1),select(Y,Rest1,Rest2),select(Z,Rest2,_).


plane(Point3, Plane) :-
Point3 = [[X1,Y1,Z1], [X2,Y2,Z2], [X3,Y3,Z3]],
    A is Y1*(Z2-Z3) + Y2*(Z3-Z1) + Y3*(Z1-Z2),
    B is Z1*(X2-X3) + Z2*(X3-X1) + Z3*(X1-X2),
    C is X1*(Y2-Y3) + X2*(Y3-Y1) + X3*(Y1-Y2),
    D is -(X1*(Y2*Z3 - Y3*Z2) + X2*(Y3*Z1 - Y1*Z3) + X3*(Y1*Z2 - Y2*Z1)),
  Plane = [A,B,C,D].

distance(Point,Plane,Dist):-
Point=[X,Y,Z],Plane=[A,B,C,D],
Dist is abs(A*X+B*Y+C*Z+D)/sqrt(A*A+B*B+C*C).

select_points(_,[],_,[],0).
select_points(Plane,[Point|Points],Eps,[Point|Selected],N):-
distance(Point,Plane,Dist),
Dist =< Eps,
N1 is N-1,
select_points(Plane,Points,Eps,Selected,N1).

select_points(Plane,[_|Points],Eps,Selected,N):-
select_points(Plane,Points,Eps,Selected,N).

support(Plane,Points,Eps,N):-
plane(Points,Plane),
select_points(Plane,Points,Eps,Selected,N),
length(Selected,N).

ransac_number_of_iterations(Confidence, Percentage, N) :-
N is ceil(log(1-Confidence)/log(1-Percentage^3)).

:- begin_tests(random3points).

test(random3points_test_1):-
Points=[[1,1,1],[2,2,2],[3,3,3]],
random3points(Points,[[1,1,1],[2,2,2],[3,3,3]]).

test(random3points_test_2):-
Points=[[1,1,1],[2,2,2],[3,3,3]],
random3points(Points,[[3,3,3],[2,2,2],[1,1,1]]).

test(random3points_test_3):-
Points=[[1,1,1],[2,2,2],[3,3,3]],
random3points(Points,[[1,1,1],[3,3,3],[2,2,2]]).

test(random3points_test_4):-Points=[],\+ random3points(Points,_).

:-end_tests(random3points).


:- begin_tests(plane).


test(plane_test_1):-plane([[2,3,4],[1,3,2],[4,2,1]],[-2,-7,1,21]).
test(plane_test_2):-plane([[1,5,3],[4,3,9],[2,8,4]],[-20,3,11,-28]).

:-end_tests(plane).


:-begin_tests(support).

test(support_test_1):-support([-2,-7,1,21],[[2,3,4],[1,3,2],[4,2,1]],0.1,3).
test(support_test_2):-support([-20,3,11,-28],[[1,5,3],[4,3,9],[2,8,4]],0.1,3).
:-end_tests(support).

:-begin_tests(ransac_number_of_iterations).

test(ransac_number_of_iterations_test_1):-
ransac_number_of_iterations(0.99,0.05,36840).

test(ransac_number_of_iterations_test_2):-
ransac_number_of_iterations(0.99,0.20,574).
:-end_tests(ransac_number_of_iterations).
