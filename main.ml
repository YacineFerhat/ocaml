open Prop_def;;
 
open List;;
open Prop_lexer;;


let rec fnc  term =
	match term with
		(Var x) 		 ->  
						 print_string x
		|Vrai 			 ->      
						 print_string "vrai";
		|Faux  			 ->     
						 print_string "faux";
		|NEG Vrai 			 ->      
						 print_string "faux";
		|NEG Faux  			 ->     
						 print_string "vrai";
		|OU( (ET(x,y)), z) -> (
						 fnc( ET( OU(x,z), OU(y,z) ) );			
						)

		|NEG OU(x,y) 		 ->   (
						 fnc (ET( (NEG x), (NEG y) ) );
						)

		|NEG ET(x,y) 		 ->   (
						 fnc (OU( (NEG x), (NEG y) ) );
						)
    |NEG IMPLIQ(x,y) -> (
             fnc ( ET( x, (NEG y)) );
						)          
		|NEG NEG x			 -> ( 
						 fnc x;
						)
		|OU(x,y) 		 ->   (
             print_string "(";
             fnc x;
             print_string "#";
             fnc y;
             print_string ")"; 
						)             
		|ET(x,y)  	 ->	(
             print_string "("; 
             fnc x;
             print_string "&";
             fnc y;
             print_string ")"; 
						)             
		|IMPLIQ(x,y) -> (
             fnc ( OU( (NEG x), y) );
						)             
		|NEG x			 -> (
						 print_string "~"; 
						 fnc x;
						)
;;
 

let boucle in_channel =
	let lexbuffer = Lexing.from_channel in_channel in
		let lire_prop_expr () = 
		Prop_parser.programme Prop_lexer.token lexbuffer in
			let p = lire_prop_expr () in 
				let k p = 			
			  print_string "\n\t**********  fnc \n";
				fnc p;
		  	print_string "\n\t********** print_term\n";
				print_term p;
		  	print_string "\n\t**********\n";	in 
					k p ;
	exit 0;;

boucle stdin;;




