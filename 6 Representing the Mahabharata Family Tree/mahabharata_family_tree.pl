%% ancestor, descendant line sibling(X,X)



male(parashara).
female(satyavati).
male(santanu).
female(ganga).

male(bhishma).
female(sudri).
male(vyasa).
male(vichitravirya).
female(ambika).
female(ambalika). 

male(dhritrashtra).
male(vidura).
male(pandu).
female(madri).
female(kunti).
female(gandhari).
male(indra).
male(vayu).
male(dharma).
male(surya).

male(kaurava).
male(duryodhana).
male(nakula).
male(sahdeva).
male(arjuna).
male(bhima).
male(yudhishthira).
male(karna).
female(draupadi).
female(subhadra).
female(ulupi).
female(chitrangada).


male(satatika).
male(srutakarma).
male(sutasoma).
male(prativindhya).
male(srutakirti).
male(abhimanyu).
male(iravan).
male(babhruvahna).
female(uttara).

male(parikshit).
female(iravati).

male(janmejaya).

%% -----------------------------------------------------------------------------------------------------------

%% child(x,y) means x is the child of y

%% married(parashara,satyavati). they did not marry
married(santanu,satyavati).
married(santanu,ganga).

married(vichitravirya,ambika).
married(vichitravirya,ambalika).
child(vyasa,parashara).
child(vyasa,satyavati).
child(vichitravirya,satyavati).
child(vichitravirya,santanu).
child(bhishma,ganga).
child(bhishma,santanu).


married(dhritrashtra,gandhari).
married(pandu,madri).
married(pandu,kunti).
child(vidura,vyasa).
child(vidura,sudri).
child(dhritrashtra,ambika).
child(dhritrashtra,vyasa).
child(dhritrashtra,vichitravirya).
child(pandu,ambalika).
child(pandu,vyasa).
child(pandu,vichitravirya).


married(nakula,draupadi).
married(sahdeva,draupadi).
married(arjuna,draupadi).
married(bhima,draupadi).
married(yudhishthira,draupadi).
married(arjuna,subhadra).
married(arjuna,ulupi).
married(arjuna,chitrangada).
child(kaurava,gandhari).
child(kaurava,dhritrashtra).
child(duryodhana,gandhari).
child(duryodhana,dhritrashtra).
child(karna,kunti).
child(karna,surya).
child(karna,pandu).
child(yudhishthira,kunti).
child(yudhishthira,pandu).
child(yudhishthira,dharma).
child(bhima,kunti).
child(bhima,pandu).
child(bhima,vayu).
child(arjuna,kunti).
child(arjuna,pandu).
child(arjuna,indra).
child(nakula,kunti).
child(nakula,pandu).
child(sahdeva,kunti).
child(sahdeva,pandu).


married(abhimanyu,uttara).
child(satatika,draupadi).
child(satatika,nakula).
child(srutakarma,draupadi).
child(srutakarma,sahdeva).
child(sutasoma,draupadi).
child(sutasoma,bhima).
child(prativindhya,draupadi).
child(prativindhya,yudhishthira).
child(srutakirti,draupadi).
child(srutakirti,arjuna).
child(abhimanyu,subhadra).
child(abhimanyu,arjuna).
child(iravan,ulupi).
child(iravan,arjuna).
child(babhruvahna,chitrangada).
child(babhruvahna,arjuna).


married(parikshit,iravati).
child(parikshit,uttara).
child(parikshit,abhimanyu).

child(janmejaya,iravati).
child(janmejaya,parikshit).


%% ---------------------------new functions --------------------- 

%% x is a father of y
father(X,Y) :- male(X),child(Y,X).

%% x is the mother of y
mother(X,Y) :- female(X),child(Y,X).

%% x is the son of y
son(X,Y) :- male(X),child(X,Y).

%% x is the daughter of y
daughter(X,Y) :- female(X),child(X,Y).

%% x is a sibling of y -- include real siblings, half brothers/sisters   (include cousins,)
%% sibling(X,Y) :- father(Z1,X),father(Z2,Y),Z1=Z2.
%% sibling(X,Y) :- mother(Z1,X),mother(Z2,Y),Z1=Z2.
sibling(X,X) :- false,!.
sibling(X,Y) :- child(X,Z),child(Y,Z).
sibling(X,Y) :- child(X,Z1),child(Y,Z2),sibling(Z1,Z2).

%% x is the uncle of y
uncle(X,Y) :- male(X),child(Y,Z),sibling(Z,X).

%% x is the aunt of y ... marraige is not the criteria here 
aunt(X,Y) :- female(X),child(Y,Z),sibling(Z,X).

%% x is the grand child of y
grandchild(X,Y) :- child(X,Z),child(Z,Y).

%% x is the grand father of y
grandfather(X,Y) :- male(X),grandchild(Y,X).

%% x is the grand mother of y
grandmother(X,Y) :- female(X),grandchild(Y,X).

%% x is the descendant of y
descendant(X,Y) :- child(X,Y).
descendant(X,Y) :- child(X,Z),descendant(Z,Y).

%% x is the ancestor of y
ancestor(X,Y) :- father(X,Y).
ancestor(X,Y) :- mother(X,Y).
ancestor(X,Y) :- child(Y,Z),ancestor(X,Z).



%% ********************************************************************************************* 

append([],L2,L2).
append(L1,[],L1).
append([X|XS],L2,[X|Z]) :- append(XS,L2,Z).

prefix([],[]).
prefix([],L).
prefix([X|XS],[X|YS]) :- prefix(XS,YS).
