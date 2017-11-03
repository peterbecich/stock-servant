
frontend:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp browserify --to static/Main.js

watch_frontend:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch browserify --to static/Main.js

watch_frontend_prod:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch build -O --to static/Main.js
