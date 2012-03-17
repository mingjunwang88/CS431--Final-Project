/************************
Mingjun Wang
CSC431 Final Project
2012 Spring
Dr. Pierro
**************************/
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
//Addition of two matrix
Matrix operator+(Matrix& c, Matrix& d){
	Matrix M(c.rows,c.cols) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=c(i,j)+d(i,j) ;
  return M ;
}
//Minus of two matrix
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
			M(i,j)=-c(i,j) ;
  return M ;
}
//Transpose of a matrix
Matrix transpose(Matrix& c){
	Matrix M(c.cols,c.rows) ;
	for (int i=0; i<c.rows;i++)
		for (int j=0;j<c.cols;j++)
			M(i,j)=c(j,i);
  return M ;
}
int is_almost_symetric(Matrix& m,float ap=1e-6,float rp=1e-4){
	//1: symetric 0: not symetric
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
int is_almost_zero(Matrix& m,float ap=1e-6,float rp=1e-4){
//1: Almost zero 
//0: not almoast zero 
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
//inverse of a matrix
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
//define norm function for float digit
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
// exp method for any double vars
Matrix exp(double x,double ap=1e-6, double rp=1e-4,int ns=40){
   double s=0, t=x;
  for (int k=0; k<ns;k++){
	  t=-1*t*x*x*(2.0*k+3)/(2.0*k+1)*(k+1.0));
	  s=s+t ;
	  if (norm(t)<ap || norm(t)<(norm(s)*rp))
		  return s;
  }
  cout<<" No Converge";
}
Matrix Cholesky(Matrix A){
	double p;
	Matrix L(A.rows,A.cols);
 if (is_almost_symetric(A)==0)
    throw string("The amtrix is not Symetric");
 for (int i=0;i<A.rows;i++)
	 for (int j=0;j<A.cols;j++)
		 L(i,j)=A(i,j);
 for (int k=0; k<L.cols;k++){
	 if (L(k,k)<=0)
		 throw string("Not positive defibite");
	  p=sqrt(L(k,k));
	  for (int i=k+1;i<L.rows;i++)
		  L(i,k)/=p;
	  for (int j=k+1;j<L.rows;j++)
		  p=L(j,k);
	  for (int i=k+1;i<L.rows;i++)
		  L(i,j)-=p*L(i,k);
 }
 for (int i=0;i<L.rows;i++)
	 for (int j=0;i<L.cols;j++)
		 L(i,j)=0 ;
    return L; 
}
int is_positive_definite(Matrix& A){
    if (!is_almost_symetric(A))
        return 0 ;
	else if (norm(Cholesky(A)))
        return 1 ;
	else return 0 ;
}
Matrix Markovitz(Matrix& mu, Matrix& A, double r_free){
//mu: average return of each risky asset;
//A:  coverance matrix
	double sum=0;
   Matrix x(A.rows,1),r_port(1,1), risk_port(1,1);
   for (int i=0; i<A.rows;i++)
	   mu(i,0)=mu(i,0)-r_free;
   x=inv(A)*mu;
   //nomorlize the weight
   for (int j=0;j<A.rows;j++)
	   sum=sum+x(j,0);
   for (int k=0;k<A.rows;k++)
	   x(k,0)=x(k,0)/sum;
    r_port=transpose(x)*mu;
	risk_port=x*A*x;
    cout<<"The total return is: "<<r_port(0,0);
	cout<<"The total risk is "<<risk_port(0,0);
	return x ; 
}
Matrix it_least_squares(double* x, double* y, int poly, double dy) {
	int size=sizeof(x)/sizeof(x[0]);
	Matrix A(size,poly);
	Matrix B(size,1);
	Matrix C(poly+1,1);
	for (int i=0;i<size;i++){
		B(i,0)=y[i]/dy;
		A(i,0)=1.0/dy;
		for (int j=1;j<poly;j++){
        A(i,j)=pow(x[i],j);
		}
	}
	C=inv(transpose(A)*A)*transpose(A)*B;
	return C;
}
//Define a function for f(x)=0;
double f(double x ){	
	//return sin(x)-0.7 ;
	return (x-2.0)*(x+8.0);
}
//Define a function for x=g(x)
float g(float x ){
	return sin(x)-0.7+x;
}
//First derivative function
float df(double x, double h=1e-5){
   return (f(x+h)-f(x-h)/(2*h));
}
//Second derivative function
double ddf(double x, double h=1e-5){
	return (f(x+h)-2.0*f(x)+f(x-h))/(h*h);
}
// Newton method: for the function differenable 
float solve_newton(float x, float ap=0.000006, float rp=0.00004, int ns=20, float h=0.00001){
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
float solve_secant(float x, float ap=0.000006, float rp=0.00004, int ns=20, float h=0.1){
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
// bisection method: for not contionus function
float solve_bisection( float a ,float b, float ap=0.000006, float rp=0.00004, int ns=100, float h=0.1){
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
		if (fx==0 || norm(b-a) < max(ap,norm(x)*rp))
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
float solve_newton_stablized( float a ,float b, float ap=0.000006, float rp=0.00004, int ns=100, float h=0.00001){
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
float solve_fixed_point(float x, float ap=1e-6, float rp=1e-4, float ns=100){
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
double optimize_bisection( double a ,double b, double ap=0.000006, double rp=0.00004, int ns=100, double h=0.1){
    double x,da,db, dx;
	da=df(a);db=df(b);
	if (da==0)
		return a;
	if (db==b)
		return b ;
	if ((da*db)>0) {
		throw string("Wrong range");
	}
	for (int i=0; i<ns;i++){
		x=(a+b)/2.0 ;
		dx=df(x) ;
		if (dx==0 || norm(b-a) < max(ap,norm(x)*rp))
			return x ;
		else if (dx*df(a)<0){
			b=x ; db=dx;
		}
		else {
			a=x; da=dx;
		}
		cout<<x<<"\n";
    }
  throw string("No Convergence");
}
// Newton method: for the function differenable 
double optimize_newton( double x, double ap=0.000006, double rp=0.00004, int ns=20, double h=0.00001){
    double dx,ddx,x_old ;
	for (int i=0; i<ns;i++){
	  dx=df(x); ddx=ddf(x);
	  x_old=x ;
	  if (dx==0) return x ;
	  if (norm(ddx)<ap) {
		  throw string("unstable solution");
	  }
	  x=x-dx/ddx;
	  cout<<x<<"\n";
	  if (norm(x-x_old)<ap || norm(x-x_old)<norm(x)*rp)
		  return x ;
   }
  throw string("No convergence");
}
// Secant method
double optimize_secant(double x, double ap=0.000006, double rp=0.00004, int ns=20, double h=0.1){
    double dx,ddx,x_old,dx_old,ddx_old;
	dx=df(x) ;//initial dx 
	ddx=ddf(x) ;//inital ddx
	cout<<x<<" "<<" "<<dx<<" "<<ddx;
	for (int i=0; i<ns;i++){
	if(dx==0) return x ;
	if (norm(ddx)<ap) {
    throw string("Unstable Aolution");
	}
	  x_old=x ;
	  dx_old=dx ;
	  ddx_old=ddx ;
	  x=x-dx/ddx; //Still use Newton
	  dx=df(x) ;
	  ddx=(dx-dx_old)/(x-x_old);
	  cout<<i<<":"<<x<<"\n";
	  if (norm(x-x_old)<ap || norm(x-x_old)<norm(x)*rp)
		  return x ;
   }
  throw string("No Convergence") ;
}
// Newton stablized method: always work for any case
double optimize_newton_stablized( double a ,double b, double ap=0.000006, double rp=0.00004, int ns=100, double h=0.00001){
    double x,da,dx,ddx,db,x_old,dx_old,ddx_old;
	da=df(a);db=df(b);
	if (da==0)
		return a;
	if (db==b)
		return b ;
	if ((da*db)>0) {
    throw string("Wrong range");
	}
	x=(a+b)/2.0 ;
	dx=df(x) ;
	ddx=ddf(x) ;
	for (int i=0; i<ns;i++){
		x_old=x; dx_old=dx, ddx_old=ddx ;
		//Do newton first
		if (norm(ddx)>ap)
			x=x-dx/ddx ;
		//If newton failed, then use bisection
		if(x_old==x ||x<a||x>b)
			x=(a+b)/2.0 ;
		if (dx==0 || norm(x-x_old) <ap || norm(x-x_old)<norm(x-x_old)*rp)
			return x ;
		//update dx and ddx
		dx=df(x);
		ddx=(dx-dx_old)/(x-x_old);
		if (dx*da<0){
			b=x ; db=dx ;
		}
		else{
			a=x;da=dx;
		}	
		cout<<x<<"\n";
    }
  throw string("No Convergence");
}
double minimize(double a,double b, double ap=1e-6, double rp=1e-4, int ns=100){
 double fa,fb,x1,x2,fx1,fx2;
 fa=f(a);
 fb=f(b);
 for (int i=0 ; i<ns;i++){
	 x1=a+(b-a)/3.0;
	 x2=a+(b-a)*2.0/3.0;
	 fx1=f(x1);
	 fx2=f(x2);
	 cout<<x1<<" "<<x2<<" "<<fx1<<" "<<fx2<<"\n";
	 if (fx1<fx2)
		 b=x2;
	 else 
		 a=x1;
	 if(norm(a-b)<max(ap,abs(a)*rp))
		 return x1 ;
 }
 throw string("No Convergence");
}
double optimize_golden_search(double a,double b, double ap=1e-6, double rp=1e-4, int ns=100){
 double fa,fb,x1,x2,fx1,fx2,tau;
 fa=f(a);
 fb=f(b);
 tau=(sqrt(5.0)-1.0)/2.0;
 x1=a+(b-a)*(1.0-tau);
 x2=a+(b-a)*tau;
 fx1=f(x1);
 fx2=f(x2);
 for (int i=0 ; i<ns;i++){
	 cout<<x1<<" "<<x2<<" "<<fx1<<" "<<fx2<<"\n";
	 if (fx1<fx2){
		 b=x2;
	     x2=x1;
		 fx2=fx1;
		 x1=a+(b-a)*(1.0-tau);
		 fx1=f(x1);
	 }
	 else {
		 a=x1;
		 x1=x2;
		 fx1=fx2;
		 x2=a+(b-a)*tau;
		 fx2=f(x2);
	 }
	 if(norm(a-b)<max(ap,abs(a)*rp))
		 return x1 ;
 }
 throw string("No Convergence");
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
 //x=solve_newton(2.001);
 //x=solve_secant(2.001);
 //x=solve_bisection(1.0,3.0);
 //x=solve_newton_stablized(1.0,3.0);
 //x=fixed_point(1.0);
 //x=minimize(-10,10) ;
 x=optimize_golden_search(-10, 10);

 cout<<"The final answer is: "<<x ;
 return 0;
}