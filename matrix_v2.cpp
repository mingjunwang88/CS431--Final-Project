#include <stdio.h>
#include <iostream>
#include <vector>
using namespace std;

class Matrix {
public:
	int rows;
	int cols;
	vector<float> data ;
public: 
	//Gerneal constructor
	Matrix(int r, int c){
	 this->rows=r;
	 this->cols=c;
	 this->data.resize(r*c);
	 for(int i=0;i<r;r++)
	  for (int k=0;k<c;c++)
		  this->data[i*c+k]=0;
	}

	//Identity constructor fill should be 1.0
	Matrix(int r){
	 this->rows=r;
	 this->cols=r;
	 this->data.resize(r*r);
	 for(int i=0;i<r;r++)
	  for (int k=0;k<r;k++)
		  this->data[i*r+k]=0;
	//have the diagnal to be one 
	for(int i=0;i<r;r++)
		  this->data[i*r+i]=1.0;
	}

	//diagonal constructor. Array fill should have the size of r
	Matrix(int r, float* fill){
	 this->rows=r;
	 this->cols=r;
	 this->data.resize(r*r);
	 for(int i=0;i<r;r++)
	  for (int k=0;k<r;k++)
		  this->data[i*r+k]=0;
	//have the diagnal to be one 
	for(int i=0;i<r;r++)
		  this->data[i*r+i]=fill[i];
	}

  float operator()(int i, int j) const {
    return data[i*cols+j];
  }  
  float &operator()(int i, int j) {
    return data[i*cols+j];
  }
  
  float getItem(int i, int k){
   return this->data[i*cols+k] ;
  }   
  void setItem(float t,int i, int k){
   this->data[i*cols+k]=t ;
  }
  //return a row 
  float* getRow(int r){
   float* p=new float[cols];
   for (int i=0;i<cols;i++)
	   p[i]=r*cols+i;
   return p ;
  }
  //return a column
  float* getCol(int c){
  float* p=new float[rows];
  for (int i=0;i<rows;i++)
	  p[i]=i*cols+c ;
  return p ;
  }
} ;

//Addition
Matrix operator+(Matrix& c, Matrix& d){
	Matrix M(c.rows,c.cols) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=c(i,j)+d(i,j) ;
  return M ;
}
//Minus
Matrix operator-(Matrix& c, Matrix& d){
	Matrix M(c.rows,c.cols) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=c(i,j)-d(i,j) ;
  return M ;
}
//product of scallar and a Matrix 
Matrix operator*(Matrix& d,float a){
	Matrix M(d.rows,d.cols) ;
	for (int i=0; i<d.rows;i++)
		for (int j=0;j<d.cols;j++)
			M(i,j)=a*d(i,j);
  return M ;
}
//product of the two matries
Matrix operator*(Matrix& c, Matrix& d){
	Matrix M(c.rows,d.cols) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<d.cols;j++)
			for (int k=0;k<c.cols;k++)
			M(i,j)+=c(i,k)*d(k,j) ;
  return M ;
}
//negative of a matrix
Matrix neg(Matrix& c){
	Matrix M(c.rows,c.cols) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=-c(i,j)*c(i,j) ;
  return M ;
}
//transpose 
Matrix transpose(Matrix& c){
	Matrix M(c.cols,c.rows) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=c(j,i);
  return M ;
}

//1: symetric 0: not symetric
int is_almost_symetric(Matrix& m,float ap=1e-6,float rp=1e-4){
	float delta ;
	if (m.rows !=m.cols){
		cout<<" Not a square matrix";
		return 0 ;
	}
	for(int i=0;i<m.rows;i++)
		for(int j=0;j<m.cols;j++){
			delta=abs(m(i,j)-m(j,i));
			if ( delta>ap && (delta>(abs(m(i,j))*rp) || delta>(abs(m(j,i))*ap)) )
				return 0 ;
		}
  return 1 ;
}
//1: alomoat zero 0: not almoast zero 
int is_almost_zero(Matrix& m,float ap=1e-6,float rp=1e-4){
	float delta ;
	for(int i=0;i<m.rows;i++)
		for(int j=0;j<m.cols;j++){
			delta=abs(m(i,j)-m(j,i));
			if ( delta>ap && (delta>(abs(m(i,j))*rp) || delta>(abs(m(j,i))*ap)) )
				return 0 ;
		}
  return 1 ;
}

void swap(float& a, float& b){
float c ;
c=a ; a=b; b=c ;
}
//inverse
Matrix inv(Matrix& A) {
  if(A.cols!=A.rows)
    cout << "BAD\n";
  Matrix B(A.rows,A.cols);
  float p;
  float q;
  int m;
  for(int r=0; r<B.cols;r++) B(r,r)=1;
  for(int c=0; c<A.cols;c++) {    
    m=c; p=A(c,c);
    for(int i=c+1; i<A.rows; i++)
      if(abs(A(i,c)) > abs(p)) {m=i; p=A(i,c);}
    for(int i=0; i<A.cols; i++) {
      swap(A(m,i),A(c,i));
      swap(B(m,i),B(c,i));
    }
    for(int i=0; i<A.cols; i++) {
      A(c,i) /= p; 
      B(c,i) /= p;
    }
    for(int r=0; r<A.rows; r++) 
      if(r!=c) {
	q = A(r,c);
	for(int i=0; i<A.cols; i++) {
	  A(r,i)-=q*A(c,i);
	  B(r,i)-=q*B(c,i);
	}
      }
  }
  return B;

}
//define norm function for float
float norm(float x){
	return abs(x) ;
}
//define norm function for Array
float norm(float x[], int p, int n){
	float sum=0.0;
	float t ;
	for (int i=0; i<n;i++)
	sum=sum + pow(abs(x[i]),p) ;
	t=1.0/p ;
	return pow(sum,t);
}
//For a matrix only one norm, max of sum of each cols;
float norm(Matrix& m){
  float sum=0, t;
  float* p=new float[m.cols] ;
  for(int i=0; i<m.cols;i++){
	  for(int j=0; j<m.rows;j++){
		  sum=sum+abs(m(j,i));
	  }
	  p[i]=sum ;
	  sum=0 ;
  }
//now have the largest sum
  t=p[0];
  for (int i=1; i<m.cols;i++){
	  if (p[i]>t)
		  t=p[i];
  }
  return t ;
}
//Matrix condition number 
float condition_number(Matrix& m){
return norm(m)*norm(inv(m)) ;
}
// exp method for a matrix
Matrix exp(Matrix& x,float ap=1e-6, float rp=1e-4,int ns=40){
  Matrix t(x.rows), s(x.rows) ; //two identity square matrix ;
  for (int k=0; k<ns;k++){
	  t=t*x*(1.0/k);
	  s=s+t ;
	  if (norm(t)<ap || norm(t)<(norm(s)*rp))
		  return s;
  }
  cout<<" No Converge";
}

void it_least_squares() {

}

//Define a function
float f(float x ){
	return sin(x)-0.7 ;
}
float g(float x ){
	return sin(x)-0.7+x;
}

// Newton method: for the function differenable 
float newton(float x, float ap=0.000006, float rp=0.00004, int ns=20, float h=0.00001){
    float df, x_old ;
	for (int i=0; i<ns;i++){
	  x_old=x ;
	  df=(f(x+h)-f(x-h))/(2.0*h) ;
	  if (norm(df)<ap) {
		  cout <<"unstable solution";
		  return 0.0 ;
	  }
	  x=x-f(x)/df;
	  cout<<x<<"\n";
	  if (norm(x-x_old)<ap || norm(x-x_old)<norm(x)*rp)
		  return x ;
   }
	cout<"No Convergence";
	return 0.0 ;
}
// Secant method
float secant(float x, float ap=0.000006, float rp=0.00004, int ns=20, float h=0.1){
    float df, x_old, df_old, fx, fx_old;
	df=(f(x+h)-f(x-h))/(2.0*h) ;//initial df 
	fx=f(x); //initial fx
	cout<<df<<" "<<fx;

	for (int i=0; i<ns;i++){
	if (norm(df)<ap) {
		  cout <<"unstable solution";
		  return 0.0 ;
	  }
	  x_old=x ;
	  fx_old=fx ;
	  x=x-fx/df;	
	  fx=f(x);
	  df=(fx-fx_old)/(x-x_old);
	  cout<<i<<":"<<x<<"\n";
	  if (norm(x-x_old)<ap || norm(x-x_old)<norm(x)*rp)
		  return x ;
   }
	cout<"No Convergence";
	return 0.0 ;
}
// bisection method: for not contionus
float bisection( float a ,float b, float ap=0.000006, float rp=0.00004, int ns=100, float h=0.1){
    float x,fa, fx, fb;
	fa=f(a);fb=f(b);
	if (fa==0)
		return a;
	if (fb==b)
		return b ;
	if ((fa*fb)>0) {
		cout<<"Wrong range";
		return 0.0 ;
	}
	for (int i=0; i<ns;i++){
		x=(a+b)/2.0 ;
		fx=f(x) ;
		if (fx==0 || norm(b-a) <ap || norm(b-a)<norm(x)*rp)
			return x ;
		else if (fx*f(a)<0){
			b=x ; fx=fb;
		}
		else {
			a=x; fx=fb;
		}
		cout<<x<<"\n";
    }
	cout<"No Convergence";
	return 0.0 ;
}
// Newton stablized method: always work for any case
float newton_stablized( float a ,float b, float ap=0.000006, float rp=0.00004, int ns=100, float h=0.00001){
    float x,fa,fx,fb,df,x_old,fx_old;
	fa=f(a);fb=f(b);
	if (fa==0)
		return a;
	if (fb==b)
		return b ;
	if ((fa*fb)>0) {
		cout<<"Wrong range";
		return 0.0 ;
	}
	x=(a+b)/2.0 ;
	fx=f(x) ;
	df=(f(x+h)-f(x-h))/(2*h);
	for (int i=0; i<ns;i++){
		x_old=x; fx_old=fx ;
		//Do newton first
		if (norm(df)>ap)
			x=x-fx/df ;
		//If newton failed, then use bisection
		if(x_old==x ||x<a||x>b)
			x=(a+b)/2.0 ;
		if (fx==0 || norm(x-x_old) <ap || norm(x-x_old)<norm(x-x_old)*rp)
			return x ;
		//update fx and df 
		fx=f(x);
		df=(fx-fx_old)/(x-x_old);
		if (fx*fa<0){
			b=x ; fb=fx ;
		}
		else{
			a=x;fa=fx;
		}	
		cout<<x<<"\n";
    }
	cout<"No Convergence";
	return 0.0 ;
}
 float fixed_point(float x, float ap=1e-6, float rp=1e-4, float ns=100){
	 float x_old ;
     for (int i=0; i<ns;i++){
         x_old=x ;
		 x=g(x) ;
        if (norm(x_old-x)<ap||norm(x_old-x)<norm(x_old-x)*rp)
            return x ;
		cout<<x<<"\n" ;
	 }
	cout<<"no convergence";
	return 0.0 ;
}
int main() {
 /* Matrix A(3,3);
  Matrix B(3,3);
  A(0,0)=1; A(0,1)=2; A(0,2)=3;
  A(1,0)=1; A(1,1)=0; A(1,2)=3;
  A(2,0)=2; A(2,1)=2; A(2,2)=4;
  //B = inv(A);
  cout << B*A << endl;*/

 float x ;
 //x=newton(2.001);
 //x=secant(2.001);
 //x=bisection(1.0,3.0);
 //x=newton_stablized(1.0,3.0);
 x=fixed_point(1.0);

 cout<<"The final answer is: "<<x ;
 int i; ;
 cin>>i ;
  return 0;
}