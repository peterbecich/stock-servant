frontend_dev:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp browserify --main MainDev --to MainDev.js

watch_frontend_dev:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch browserify --main MainDev --to MainDev.js



frontend_prod:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp browserify --main Main --to Main.js

watch_frontend_prod:
	cd stock-frontend && \
	npm install  && \
	bower install && \
	pulp build && \
	pulp --watch browserify --main Main --to Main.js
