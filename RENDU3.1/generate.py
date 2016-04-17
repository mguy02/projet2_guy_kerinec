#!/usr/bin/python3
# -*-coding:latin-1 -*

import random
import time

l=[]
operateurs = [" /\ ", " \/ ", " X ", " => ", " <=> "]

fichier = open("numeroTests", "r")
lt = (fichier.read()).split("\n")
numTestCNF = lt[0]
numTestFOR = lt[1]
fichier.close()
	


def generateFormula(indmax, nbLitt):
	global operateurs
	output = ""
	
	for i in range(nbLitt):
		negation = random.randint(0,1)
		if negation == 1:
			output += " ~ "
		litt1 = random.randint(1, indmax)
		op = operateurs[random.randint(0, len(operateurs)-1)]
		output += str(litt1) + op
		
	output = output[:len(output)-4]
	
	if (output[len(output)-1] == " "):
		output = output[:len(output)-1] 
	return(output+" 0")

def generateClause (indmax, nbClause, nbLitt):
	global l
	if (nbLitt == 0):
		nbLitt = random.randint(1, indmax)
	output = ""
	lr = random.sample(l, nbLitt)
	while lr != []:
		litt = lr.pop()
		output += " "*(len(str(indmax))-len(str(abs(litt))))
		if (litt < 0):
			output += "-"
		else:
			output += " "
		output += str(abs(litt)) + " "		
	return(output+"0\n")
	
choix = 0

while (choix != 1 and choix != 2):
	choix = int(input("Que generer\n1- Fichier .cnf\n2- Fichier .for\n"))

if choix == 1:
	indmax = int(input("Indice max: "))
	nbClause = int(input("Nombre de clauses: "))
	nbLitt = int(input("Nombre de litteraux par clause (0 pour random): "))
	
	fichier = open("numeroTests", "w")
	fichier.write(str(int(numTestCNF)+1)+"\n")
	fichier.write(str(numTestFOR))
	fichier.close()
	
	
	l = [i+1 for i in range(indmax)] + [-(i+1) for i in range(indmax)]
	
	nomFichier = "tests/generated/generated_"+str(numTestCNF)+".cnf"
	
	output = "c FILE: " + nomFichier + "\n"
	output += "c\n"
	output += "c Generated on " + time.strftime('%B %d %Y - %H:%M:%S') + "\n"
	output += "c\nc\n"
	
	output += "p cnf " + str(indmax) + " " + str(nbClause) + "\n"
	#c p cnf V C avec V indice max des variables utilisees et C nombre de clauses
	
	for i in range(nbClause):
		output += generateClause(indmax, nbClause, nbLitt)
	
	
	#Puis on ecrit output dans un fichier
	
	fichier = open(nomFichier, "w")
	
	fichier.write(output)
	
	fichier.close()

	print("Fichier", nomFichier, "genere avec succes.")	
	
elif choix == 2:
	fichier = open("numeroTests", "w")
	fichier.write(str(numTestCNF)+"\n")
	fichier.write(str(int(numTestFOR)+1))
	fichier.close()

	indmax = int(input("Indice max: "))
	nbLitt = int(input("Nombre de litteraux: "))
	
	output = generateFormula(indmax, nbLitt)

	nomFichier = "tests/generated/tseitin/generated_"+str(numTestFOR)+".for"
	fichier = open(nomFichier, "w")
	
	fichier.write(output)
	
	fichier.close()

	print("Fichier", nomFichier, "genere avec succes.")	

