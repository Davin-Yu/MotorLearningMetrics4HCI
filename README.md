# Motor Learning Metrics for HCI Research
To quantify and understand how users learn movement-based techniques in HCI, we build four metrics based on motor learning literature. Refinement time proportion (RTP) and refinement space (RS) quantify the user prediction error. Normalized path error (NPE) and normalized jerk error (NJE) measure the closeness of the current movement to an ideal movement. 

### Paper 
Difeng Yu, Mantas Cibulskis, Erik Skjoldan Mortensen, Mark Schram Christensen, and Joanna Bergström. 2024. Metrics of Motor Learning for Analyzing Movement Mapping in Virtual Reality. In Proceedings of the CHI Conference on Human Factors in Computing Systems (CHI ’24), May 11– 16, 2024, Honolulu, HI, USA. ACM, New York, NY, USA, 18 pages. https://doi.org/10.1145/3613904.3642354

### How to Install
```r
library(devtools)
install_github("Davin-Yu/MotorLearningMetrics4HCI")
```
or use our binary package in the repository
```r
install.packages(path_to_the_.tar.gz_file, repos = NULL, type="source")
```

### How to Start
1. Please read the paper to get an understanding of how the metrics work.
2. After installing the package in R, use ```help(package = "MotorLearningMetrics4HCI")``` to navigate the package. You can run the example codes included in the documentation.