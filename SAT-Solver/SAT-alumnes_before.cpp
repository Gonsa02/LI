#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0
#define THRESHOLD 5

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel; //Millorar en la occurlist les propagacions de variables
vector<pair<vector<uint>,vector<uint>>> occurList;
vector<pair<uint,uint>> priority;
int numConflicts;
vector<uint> conflictsPerVariable;
int maxim;

void output() {
  cout << "HI HA TANTES CLAUSULES: " << numClauses << endl;
  for (uint i = 0; i <= numVars; ++i) {
    cout << i << endl;
    for (uint j = 0; j < occurList[i].first.size(); ++j) cout << occurList[i].first[j] << "  ";
    cout << endl;
    for (uint j = 0; j < occurList[i].second.size(); ++j) cout << occurList[i].second[j] << "  ";
    cout << endl;
  }
}

void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  
  
  // Creem la occurlist on cada posició es la variable i el pair 0 es literal positiu i el pair 1 es el literal negatiu
  for (uint i = 0; i <= numVars; ++i) {
      pair<vector<uint>, vector<uint>> aux = make_pair(vector<uint> (0), vector<uint> (0));
      occurList.push_back(aux);
  }

  vector<uint> count = vector<uint>(numVars+1, 0);
  // Read clauses
  for (uint i = 0; i <= numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      ++count[abs(lit)];
	    clauses[i].push_back(lit);
	    if (lit > 0) occurList[lit].first.push_back(i);
	    else occurList[abs(lit)].second.push_back(i);
    }
  }
  for (uint i = 1; i <= numVars; ++i) priority.push_back(make_pair(count[i], i));
  sort(priority.begin(), priority.end(), greater<>());
  numConflicts = 0;
  conflictsPerVariable = vector<uint> (numVars+1, 0);
  maxim = 1;
}



int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;
}

void updateHeuristic() {
    for (uint& i : conflictsPerVariable) i /= 2;
    maxim = 1;
}

bool propagateGivesConflict() {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    int literal = modelStack[indexOfNextLitToPropagate];
    int index = abs(literal);
    if (literal < 0) {
      uint total_aparicions_a_clausules_positives = occurList[index].first.size();
      for (uint i = 0; i < total_aparicions_a_clausules_positives; ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      uint total_literals_a_clausula_positiva = clauses[occurList[index].first[i]].size();
      for (uint k = 0; not someLitTrue and k < total_literals_a_clausula_positiva; ++k){
	      uint num_clausula = occurList[index].first[i];
	      int val = currentValueInModel(clauses[num_clausula][k]);
	      if (val == TRUE) someLitTrue = true;
	      else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[num_clausula][k]; }
      }
      if (not someLitTrue and numUndefs == 0) {
        ++numConflicts;
        for (uint k = 0; k < total_literals_a_clausula_positiva; ++k) {
          uint num_clausula = occurList[index].first[i];
	        int var = abs(clauses[num_clausula][k]);
          ++conflictsPerVariable[var];
	  if (conflictsPerVariable[var] > conflictsPerVariable[maxim]) maxim = var;
        }
        return true; // conflict! all lits false
      }
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
    }
    } else {
      uint total_aparicions_a_clausules_negatives = occurList[index].second.size();
      for (uint i = 0; i < total_aparicions_a_clausules_negatives; ++i) {
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        uint total_literals_a_clausula_negativa = clauses[occurList[index].second[i]].size();
        for (uint k = 0; not someLitTrue and k < total_literals_a_clausula_negativa; ++k){
          uint num_clausula = occurList[index].second[i];
          int val = currentValueInModel(clauses[num_clausula][k]);
          if (val == TRUE) someLitTrue = true;
          else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[num_clausula][k]; }
        }
        if (not someLitTrue and numUndefs == 0) {
          ++numConflicts;
          for (uint k = 0; k < total_literals_a_clausula_negativa; ++k) {
            uint num_clausula = occurList[index].second[i];

            int var = abs(clauses[num_clausula][k]);
            ++conflictsPerVariable[var];
	  if (conflictsPerVariable[var] > conflictsPerVariable[maxim]) maxim = var;
          }
	  if (numConflicts % 40 == 0) updateHeuristic();
          return true; // conflict! all lits false
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
      }
    }
    ++indexOfNextLitToPropagate;
  }
  return false;
}

void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}

//Després mirar primer les variables que hi ha dins la clausula que te generen conflicte

// Agafar sempre primer la variable que més apareix
// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  if (numConflicts < THRESHOLD) {
    for (uint i = 0; i < priority.size(); ++i) 
      if (model[priority[i].second] == UNDEF) return priority[i].second;
  } else {
    if (model[maxim] == UNDEF) return maxim;
    int pos = 0;
    uint max = 0;
    for (uint lit = 1; lit <= numVars; ++lit) {
      if (model[lit] == UNDEF && conflictsPerVariable[lit] > max) {
        max = conflictsPerVariable[lit];
        pos = lit;
      }
    }
    return pos;
  }
  return 0; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
	if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
	backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
