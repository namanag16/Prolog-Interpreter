%% *************************************************************************************************** %% 
edge(a,b). 
edge(a,g). 
edge(b,d). 
edge(d,c). 
edge(g,c). 
edge(g,f).
edge(c,e).
edge(e,d).
edge(h,i).

%% path(X,X).
%% path(X,Y) :- edge(X,Z),path(Z,Y).

path(A,B) :-   
  walk(A,B,[]).            

walk(A,B,V) :-       
  edge(A,X) ,  not(member(X,V)) , ( B = X ; walk(X,B,[A|V]) ) .  


cycle(X) :- edge(X,Z),path(Z,X).


%% *************************************************************************************************** %% 

female(sita).
female(urmila).
female(mom).
male(dad).
male(laxman).
male(bro1).

child(me,dad).
child(me,mom).
child(sis,mom).
child(dad,dada).
child(dada,pardada).
child(mom,nana).
child(nana,parnana).

married(dad,mom).
married(mom,dad).
married(laxman,urmila).
married(urmila,laxman).

sibling(mom,urmila).
sibling(urmila,mom).
sibling(sita,urmila).
sibling(urmila,sita).
sibling(mom,bro1).
sibling(bro1,mom).
sibling(dad, bharat).

uncle(X,Y) :- male(X),married(X,Z),sibling(Z,Z1),child(Y,Z1).
uncle(X,Y) :- male(X),sibling(X,Z),child(Y,Z).
ancestor(X,Y) :- child(Y,X).
ancestor(X,Y) :- child(Y,Z),ancestor(X,Z).


%% *************************************************************************************************** %% 

