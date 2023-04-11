# CPSC-312-Prolog-Project

## Steps to run: 

`[api].` \
`[query_question].`\
`[pokedex].`  \
`start(Ans).`  \

## Questions you can ask: 

Where is Dialga?     

What Pokemon is at the spear-pillar-area?  

What type is dialga?  

What Pokemon is fire type?    

3 ?- start(Ans).

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: q\
Ask me: Where is Dialga?\
Ans = "spear-pillar-area" ;\
Ans = "sinjoh-ruins-area" ;\
No more answers or invalid query\
Would you like to exit? no

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: q\
Ask me: What Pokemon is at the spear-pillar-area?\
Ans = ["dialga", "palkia"] ;\
No more answers or invalid query\
Would you like to exit? no

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: q\
Ask me: What type is dialga?\
Ans = "steel" ;\
Ans = "dragon" ;\
No more answers or invalid query\
Would you like to exit? no

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: q\
Ask me: What Pokemon is fire type?\
Ans = ["charmander", "charmeleon", "charizard", "vulpix", "ninetales", "growlithe", "arcanine", "ponyta", "rapidash"|...] ;\
No more answers or invalid query\
Would you like to exit? no

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: e\
Enter encounter: Pichu\
Ans = success ;\
No more answers or invalid query\
Would you like to exit? no

List of commands:\
e - Enter a new Pokemon encounter.\
q - Ask a question.\
l - List all encountered or searched Pokemon.\
c - Clear history.\
Input: l\
Ans = "dialga" ;\
Ans = "pichu" ;\
No more answers or invalid query\
Would you like to exit? yes\
true.
