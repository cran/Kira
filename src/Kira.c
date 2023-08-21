#include <R.h>     // Bibliotecas para integrar com as funcoes matematicas do R
#include <Rmath.h> // Bibliotecas para integrar com as funcoes matematicas do R
#include <math.h>  // Biblioteca matematica pardrao do C
//#include "Several.h"

 
void knn(int *row_train, int *row_test, int *col, double train[*col][*row_train], double test[*col][*row_test],  
         int *mtd, int *k, double *lambda, double neigh[]); /* knn - k-Nearest Neighbour Classification */

//void kmeans(int *row_train, int *row_test, int *col, double train[*col][*row_train], double test[*col][*row_test], int *mtd, int *k, 
//            double *lambda, double neigh[], double mdist[*row_train], double aux[2][*row_train]); /* knn - k-Nearest Neighbour Classification */


void knn(int *row_train, int *row_test, int *col, double train[*col][*row_train], double test[*col][*row_test], 
         int *mtd, int *k, double *lambda, double neigh[]) { /* knn - k-Nearest Neighbour Classification */
     // row_train - number of rows of the training set
     // row_test  - number of rows of the testing set
     // col    - number of columns of training and testing sets
     // train  - data of training
     // test   - data of testing
     // mtd    - method used in process classification
     // k      - number of nearest neighbour used in classification 
     // lambda - power used in  minkowski distance
     // neigh  - the chosen neighbors - k by k
     
    register int i, j, r = 0, s, t, u = 0; 
	register double sum, value, max_vlr = 0.0; 

    double mdist[*row_train]; // distance matrix
    
	switch(*mtd) {
		
	   case 1: { // euclidean distance
	   
	   	    for(i = 0; i < *row_test; i++) {
	                     
	         	for(j = 0; j < *row_train; j++) {
	         		
					sum = 0.0;
					
				    for(s = 0; s < *col; s++) { // calculate euclidean distance
				       sum += pow(train[s][j] - test[s][i], 2.0);
				    } 
					mdist[j] = sqrt(sum);
					
	            }
 
            	for(s = 0; s < *k; s++) { // find k nearest neighbors
            	
            	   value = mdist[0];
                   neigh[r] = 1;
 
            	   for(t = 1; t < *row_train; t++) {
            	   	  if(value > mdist[t]) {
	                    neigh[r] = t + 1;
	                    value = mdist[t];
	                    u = t;
	                  } else max_vlr = mdist[t]; // valor maximo
	               }
	               
	               mdist[u] = max_vlr + 2.0;     
	               r++;
	               
	            }
	            
	    	}
	    	
		break;
	   }
	   
	   case 2: { // manhattan distance
	   
	   	    for(i = 0; i < *row_test; i++) {
	                     
	         	for(j = 0; j < *row_train; j++) {
	         		
					sum = 0.0;
					
				    for(s = 0; s < *col; s++) { // calculate manhattan distance
				       sum += fabs(train[s][j] - test[s][i]);
				    } 
					mdist[j] = sum;

	            }
 
            	for(s = 0; s < *k; s++) { // find k nearest neighbors
            	
            	   value = mdist[0];
                   neigh[r] = 1;
                   
            	   for(t = 1; t < *row_train; t++) {
            	   	  if(value > mdist[t]) {
	                    neigh[r] = t + 1;
	                    value = mdist[t];
	                    u = t;
	                  } else max_vlr = mdist[t]; // valor maximo
	               }
	               
	               mdist[u] = max_vlr + 2.0;     
	               r++;
	               
	            }
	            
	    	}

		break;
	   }
	   
	   case 3: { // minkowski distance
	   
	   	    for(i = 0; i < *row_test; i++) {
	                     
	         	for(j = 0; j < *row_train; j++) {
	         		
					sum = 0.0;
					
				    for(s = 0; s < *col; s++) { // calculate minkowski distance
				       sum += pow(fabs(train[s][j] - test[s][i]), *lambda);
				    }   
					mdist[j] = pow(sum, 1.0/(*lambda));

	            }
 
            	for(s = 0; s < *k; s++) { // find k nearest neighbors
            	
            	   value = mdist[0];
                   neigh[r] = 1;
                   
            	   for(t = 1; t < *row_train; t++) {
            	   	  if(value > mdist[t]) {
	                    neigh[r] = t + 1;
	                    value = mdist[t];
	                    u = t;
	                  } else max_vlr = mdist[t]; // valor maximo
	               }
	               
	               mdist[u] = max_vlr + 2.0;     
	               r++;
	               
	            }
	            
	    	}
	    	
		break;
	   }
 
	   case 4: { // canberra distance
	   
	   	    for(i = 0; i < *row_test; i++) {
	                     
	         	for(j = 0; j < *row_train; j++) {
	         		
					sum = 0.0;
					
				    for(s = 0; s < *col; s++) { // calculate canberra distance
				       sum += fabs(train[s][j] - test[s][i]) / (train[s][j] + test[s][i]);
				    }   
					mdist[j] = sum;

	            }
 
            	for(s = 0; s < *k; s++) { // find k nearest neighbors
            	
            	   value = mdist[0];
                   neigh[r] = 1;
                   
            	   for(t = 1; t < *row_train; t++) {
            	   	  if(value > mdist[t]) {
	                    neigh[r] = t + 1;
	                    value = mdist[t];
	                    u = t;
	                  } else max_vlr = mdist[t]; // valor maximo
	               }
	               
	               mdist[u] = max_vlr + 2.0;     
	               r++;
	               
	            }
	            
	    	}
	    	
		break;
	   }

	   case 5: { // maximum (chebyshev) distance
	   
	   	    for(i = 0; i < *row_test; i++) {
	                     
	         	for(j = 0; j < *row_train; j++) {
	         		
					sum = 0.0;
					
				    for(s = 0; s < *col; s++) { // calculate maximum (chebyshev) distance
				       value = fabs(train[s][j] - test[s][i]);
				       if (value > sum) sum = value;
				    }   
					mdist[j] = sum;

	            }
 
            	for(s = 0; s < *k; s++) { // find k nearest neighbors
            	
            	   value = mdist[0];
                   neigh[r] = 1;
                   
            	   for(t = 1; t < *row_train; t++) {
            	   	  if(value > mdist[t]) {
	                    neigh[r] = t + 1;
	                    value = mdist[t];
	                    u = t;
	                  } else max_vlr = mdist[t]; // valor maximo
	               }
	               
	               mdist[u] = max_vlr + 2.0;     
	               r++;
	               
	            }
	            
	    	}
	    	
		break;
	   }

    }

}

