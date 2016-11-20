#include <Rcpp.h>
using namespace Rcpp;

// knapsack fraccional usando greedy

// [[Rcpp::export]]
List knapsack_fract(NumericVector weight, NumericVector profit, double capacity) {
  int n = weight.length();
  double ratio[n];
  double temp;
  int i, j;
  NumericVector vect_final(n); 
  
  double tp = 0;
  double num = n;
  
  for (i = 0; i < num; i++) {
    ratio[i] = profit[i] / weight[i];
  }
  
  for (i = 0; i < num; i++) {
    for (j = i + 1; j < num; j++) {
      if (ratio[i] < ratio[j]) {
        temp = ratio[j];
        ratio[j] = ratio[i];
        ratio[i] = temp;
        
        temp = weight[j];
        weight[j] = weight[i];
        weight[i] = temp;
        
        temp = profit[j];
        profit[j] = profit[i];
        profit[i] = temp;
      }
    }
  }
  
  for (i = 0; i < n; i++)
    vect_final[i] = 0.0;
  
  for (i = 0; i < n; i++) {
    if (weight[i] > capacity)
      break;
    else {
      vect_final[i] = 1.0;
      tp = tp + profit[i];
      capacity = capacity - weight[i];
    }
  }
  
  if (i < n)
    vect_final[i] = capacity / weight[i];
  
  tp = tp + (vect_final[i] * profit[i]);
  
  return List::create(
    Rcpp::Named("profit") = tp,
    Rcpp::Named("result") = vect_final
  );
}


// knapsack 0/1 usando greedy (función auxiliar)

// [[Rcpp::export]]
NumericVector knapsack_1_0_aux(NumericMatrix datos, int capacity) {

  int n = datos.rows();
  
  NumericVector vector_aux = NumericVector(n);
  
  int left_capacity = capacity;
  int i = 0;
  while(left_capacity > 0 & i < n){
    if(datos(i, 1) < left_capacity){
      vector_aux(i) = datos(i, 0);
      left_capacity = left_capacity - datos(i, 1);
    }
    i++;
  }
  
  return vector_aux;
}
