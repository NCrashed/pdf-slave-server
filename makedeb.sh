mkdir artifacts
# Build server
stack install --local-bin-path=artifacts
# Copy to debian build dir
mkdir -p ubuntu/data/usr/bin
cp artifacts/pdf-slave-server ubuntu/data/usr/bin
cp artifacts/pdf-slave-server-cli ubuntu/data/usr/bin
mkdir -p ubuntu/data/var/lib/pdf-slave-server
# Make deb package
fpm -f -s dir \
  -t deb \
  -n "pdf-slave-server" \
  --after-install=ubuntu/post.sh \
  --before-remove=ubuntu/pre.sh \
  -d "pdf-slave" \
  -C ubuntu/data \
  .
# Cleanup
rm -rf artifacts
