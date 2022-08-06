find pages -type f -name '*.cfg' -print0 | sort -zr | xargs -0 saait -o docs -t templates
