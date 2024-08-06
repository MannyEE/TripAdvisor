Trip Advisor
This program is a trip advisor that helps optimize flight and ground travel routes

Restrictions:
- You need a google maps API key, stored in the filepath '/home/ubuntu/api' in order for the program to find
- Since we are scraping off of kayak, each IP address is limited to 200 calls to the kayak server. Every time the user runs the program, about N * (N - 1) calls are made to kayak, with N being the number of airports the user wants to visit. This means the user could enter at most 14 airports in one request.