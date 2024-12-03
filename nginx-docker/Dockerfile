FROM nginx:latest

# Copy the custom Nginx configuration file (create this later)
COPY nginx.conf /etc/nginx/nginx.conf

# Expose the port that Nginx will use
EXPOSE 80

# Start Nginx
CMD [ "nginx", "-g", "daemon off;"]
# ENTRYPOINT [ "nginx", "-g", "daemon off;" ] 
# CMD ["nginx", "-g", "daemon off;"]