#include <Rcpp.h>
using namespace Rcpp;
#include <cmath>
#include <vector>

// grow_poly is now a helper function that operates directly on vectors
void grow_poly(std::vector<double> &x,
               std::vector<double> &y,
               std::vector<double> &point,
               std::vector<double> &iteration,
               std::vector<double> &side_variability) {

  int n = x.size();
  std::vector<double> xend(n), yend(n), side_length(n), split_point(n), split_angle(n), split_distance(n);
  std::vector<double> split_x(n), split_y(n), new_xend(n), new_yend(n);

  std::vector<double> new_x, new_y, new_point, new_iteration, new_side_variability;

  for(int i = 0; i < n; i++) {
    xend[i] = (i == n-1) ? x[0] : x[i+1];
    yend[i] = (i == n-1) ? y[0] : y[i+1];

    side_length[i] = sqrt(pow(x[i] - xend[i], 2) + pow(y[i] - yend[i], 2));
    split_point[i] = 0.5 + R::rnorm(0, 0.1);
    split_angle[i] = R::rnorm(0, 20);
    split_distance[i] = sqrt(side_length[i]) * side_variability[i];
    split_x[i] = x[i] - (x[i] - xend[i]) * split_point[i];
    split_y[i] = y[i] - (y[i] - yend[i]) * split_point[i];
    double new_angle = atan2(split_y[i] - yend[i], split_x[i] - xend[i]) + M_PI/2 + (split_angle[i] * M_PI / 180);
    new_xend[i] = split_x[i] + cos(new_angle) * split_distance[i];
    new_yend[i] = split_y[i] + sin(new_angle) * split_distance[i];

    // add original and new points to the vectors
    new_x.push_back(x[i]);
    new_y.push_back(y[i]);
    new_point.push_back(point[i] + i);
    new_iteration.push_back(iteration[i] + 1);
    new_side_variability.push_back(side_variability[i]);  // keep original variability

    // add new point and corresponding side_variability
    new_x.push_back(new_xend[i]);
    new_y.push_back(new_yend[i]);
    new_point.push_back(point[i] + i + 1);
    new_iteration.push_back(iteration[i] + 1);
    new_side_variability.push_back(R::rnorm(0.25, 0.06));  // new side variability with some randomness
  }

  // update the vectors with new points
  x = new_x;
  y = new_y;
  point = new_point;
  iteration = new_iteration;
  side_variability = new_side_variability;
}


// tightly integrated splotch function
// [[Rcpp::export]]
Rcpp::DataFrame splotch_rcpp(Rcpp::DataFrame base, int iterations = 3) {

  // extract the vectors from the base dataframe
  std::vector<double> x = Rcpp::as<std::vector<double>>(base["x"]);
  std::vector<double> y = Rcpp::as<std::vector<double>>(base["y"]);
  std::vector<double> point = Rcpp::as<std::vector<double>>(base["point"]);
  std::vector<double> iteration = Rcpp::as<std::vector<double>>(base["iteration"]);
  std::vector<double> side_variability = Rcpp::as<std::vector<double>>(base["side_variability"]);

  // perform the iterations in-place on the vectors
  for (int i = 0; i < iterations; i++) {
    grow_poly(x, y, point, iteration, side_variability);
  }

  // return the final dataframe, including side_variability
  return Rcpp::DataFrame::create(Rcpp::Named("point") = point,
                                 Rcpp::Named("iteration") = iteration,
                                 Rcpp::Named("x") = x,
                                 Rcpp::Named("y") = y,
                                 Rcpp::Named("side_variability") = side_variability);
}
