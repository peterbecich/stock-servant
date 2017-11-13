
frontend:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp browserify --to Main.js

watch_frontend:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch browserify --to Main.js

watch_frontend_prod:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch browserify -O --to Main.js
