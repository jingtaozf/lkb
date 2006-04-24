#include <db.h>
#include <stdio.h>
#include <string.h>

#define MAX_DBS 16
#define MAX_CURSORS 16
#define MAX_PARAMETERS 2

#undef DEBUG

int db_max_parameters = MAX_PARAMETERS;

struct db_key {
  int iid;
  int rid;
  int tid;
  int parameters[2];
};

struct db_data_int {
  int code;
  int count;
};

struct db_data_float {
  int code;
  float count;
};


static DB* dbs[] =
  { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

static DBC* cursors[] =
  { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

int db_open(const char *, int, int);
void db_close(int);

int db_open(const char *file, int createp, int cache) {
  
  int handle, foo;
  
  for(handle = 0; handle < MAX_DBS && dbs[handle] != NULL; ++handle);
  if(handle >= MAX_DBS) return -1;

  if((foo = db_create(&dbs[handle], NULL, 0))) {
    fprintf(stderr, "db_open(): error `%s'\n", db_strerror(foo));
    return -1;
  } // if
  DB *dbp = dbs[handle];
  
  if((foo = dbp->set_cachesize(dbp, 0, cache, 0))) {
    fprintf(stderr, "db_open(): error `%s'\n", db_strerror(foo));
    db_close(handle);
    return -1;
  } // if

  if(createp) {
    if((foo = dbp->set_flags(dbp, DB_DUP))) {
      fprintf(stderr, "db_open(): error `%s'\n", db_strerror(foo));
      db_close(handle);
      return -1;
    } // if
  }// if

  if((foo = dbp->open(dbp, NULL, file, NULL, DB_BTREE,
                      (createp ? DB_CREATE|DB_TRUNCATE : 0), 0))) {
    fprintf(stderr, "db_open(): error `%s'\n", db_strerror(foo));
    db_close(handle);
    return -1;
  } // if
     
  return handle;

} // db_open()

void db_close(int handle) {

  if(handle >= MAX_DBS) return;

  DB *dbp = dbs[handle];
  dbp->close(dbp, 0);
  dbs[handle] = NULL;

} // db_close()

void db_cursor_close(int chandle) {

  if(chandle >= MAX_CURSORS) return;

  DBC *cursor = cursors[chandle];
  if(cursor != NULL) {
    cursor->c_close(cursor);
    cursors[chandle] = NULL;
  } // if

} // db_cursor_close()

void db_create_key(struct db_key *key,
                   int iid, int rid, int tid,
                   int *parameters, int nparameters) {

  key->iid = iid;
  key->rid = rid;
  key->tid = tid;
  int i;
  for(i = 0; i < nparameters && i < db_max_parameters; ++i)
    key->parameters[i] = parameters[i];

  for(i = nparameters; i < db_max_parameters; ++i)
    key->parameters[i] = 0;

#ifdef DEBUG
  fprintf(stderr,
          "db_create_key(): [%d %d %d {%d %d}.\n",
          key->iid, key->rid, key->tid,
          key->parameters[0], key->parameters[1]);
#endif
} // db_create_key()

int db_write_feature_int(int handle,
                         int iid, int rid, int tid,
                         int *parameters, int nparameters,
                         int code, int count) {

#ifdef DEBUG
  fprintf(stderr,
          "db_write_feature_int(): %d %d %d %d {%d %d} %d %d %d.\n",
          handle, iid, rid, tid,
          parameters[0], parameters[1], nparameters, code, count);
#endif

  if(handle >= MAX_DBS) return -1;

  DB *dbp = dbs[handle];

  DBT key_dbt, data_dbt;
  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  struct db_key key;
  db_create_key(&key, iid, rid, tid, parameters, nparameters);
  key_dbt.data = &key;
  key_dbt.size = sizeof(struct db_key);

  struct db_data_int data;
  data.code = code;
  data.count = count;
  data_dbt.data = &data;
  data_dbt.size = sizeof(struct db_data_int);

  int foo;
  if((foo = dbp->put(dbp, NULL, &key_dbt, &data_dbt, 0))) {
    fprintf(stderr, "db_write_feature_int(): error `%s'\n", db_strerror(foo));
    return -1;
  } // if

  return 0;

} // db_write_feature_int()

int db_write_feature_float(int handle,
                           int iid, int rid, int tid,
                           int *parameters, int nparameters,
                           int code, float count) {


  if(handle >= MAX_DBS) return -1;

  DB *dbp = dbs[handle];

  DBT key_dbt, data_dbt;
  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  struct db_key key;
  db_create_key(&key, iid, rid, tid, parameters, nparameters);
  key_dbt.data = &key;
  key_dbt.size = sizeof(struct db_key);

  struct db_data_float data;
  data.code = code;
  data.count = count;
  data_dbt.data = &data;
  data_dbt.size = sizeof(struct db_data_float);

  int foo;
  if((foo = dbp->put(dbp, NULL, &key_dbt, &data_dbt, 0))) {
    fprintf(stderr,
            "db_write_feature_float(): error `%s'.\n",
            db_strerror(foo));
    return -1;
  } // if

  return 0;

} // db_write_feature_float()

int db_read_feature_int(int handle, int chandle,
                        int iid, int rid, int tid,
                        int *parameters, int nparameters,
                        int *code, int *count) {

  if(handle != -1 && handle >= MAX_DBS) return -1;
  if(chandle != -1 && handle >= MAX_CURSORS) return -1;

  DBC *cursor = NULL;
  int foo;

  if(chandle >= 0) {
    cursor = cursors[chandle];
  } // if
  else {
    DB *dbp = dbs[handle];

    for(chandle = 0;
        chandle < MAX_CURSORS && cursors[chandle] != NULL;
        ++chandle);
    if(chandle >= MAX_CURSORS) return -1;

    if((foo = dbp->cursor(dbp, 0, &cursors[chandle], 0))) {
      fprintf(stderr,
              "db_read_feature_int(): error `%s'.\n",
              db_strerror(foo));
      cursors[chandle] = NULL;
      return -1;
    } // if
    cursor = cursors[chandle];
  } // if

  DBT key_dbt, data_dbt;
  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  struct db_key key;
  db_create_key(&key, iid, rid, tid, parameters, nparameters);
  key_dbt.data = &key;
  key_dbt.size = sizeof(struct db_key);

  data_dbt.size = sizeof(struct db_data_int);

  if((foo = cursor->c_get(cursor, &key_dbt, &data_dbt,
                          (handle == -1 ? DB_NEXT_DUP : DB_SET)))) {
    if(foo != DB_NOTFOUND)
      fprintf(stderr,
              "db_read_feature_int(): error `%s'.\n",
              db_strerror(foo));
    db_cursor_close(chandle);
    return -1;
  }

  struct db_data_int *data = data_dbt.data;
  *code = data->code;
  *count = data->count;

  return chandle;

} // db_read_feature_int()

int db_read_feature_float(int handle, int chandle,
                          int iid, int rid, int tid,
                          int *parameters, int nparameters,
                          int *code, float *count) {

#ifdef DEBUG
  fprintf(stderr,
          "db_read_feature_float(): %d %d %d %d %d {%d %d} %d %p %p.\n",
          handle, chandle, iid, rid, tid,
          parameters[0], parameters[1], nparameters, code, count);
#endif

  if(handle != -1 && handle >= MAX_DBS) return -1;
  if(chandle != -1 && handle >= MAX_CURSORS) return -1;

  DBC *cursor = NULL;
  int foo;

  if(chandle >= 0) {
    cursor = cursors[chandle];
  } // if
  else {
    DB *dbp = dbs[handle];

    for(chandle = 0;
        chandle < MAX_CURSORS && cursors[chandle] != NULL;
        ++chandle);
    if(chandle >= MAX_CURSORS) return -1;

    if((foo = dbp->cursor(dbp, 0, &cursors[chandle], 0))) {
      fprintf(stderr,
              "db_read_feature_float(): error `%s'.\n",
              db_strerror(foo));
      cursors[chandle] = NULL;
      return -1;
    } // if
    cursor = cursors[chandle];
  } // if

  DBT key_dbt, data_dbt;
  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  struct db_key key;
  db_create_key(&key, iid, rid, tid, parameters, nparameters);
  key_dbt.data = &key;
  key_dbt.size = sizeof(struct db_key);

  data_dbt.size = sizeof(struct db_data_float);

  if((foo = cursor->c_get(cursor, &key_dbt, &data_dbt,
                          (handle == -1 ? DB_NEXT_DUP : DB_SET)))) {
    if(foo != DB_NOTFOUND)
      fprintf(stderr,
              "db_read_feature_float(): error `%s'.\n",
              db_strerror(foo));
    db_cursor_close(chandle);
    return -1;
  }

  struct db_data_float *data = data_dbt.data;
  *code = data->code;
  *count = data->count;

  return chandle;

} // db_read_feature_float()

int main(int argc, char **argv) {

  int handle = db_open((argv[1] != NULL ? argv[1] : "/tmp/bar.bdb"),
                       0, 1024 * 1024 * 1024);
  int chandle;
  DB *dbp = dbs[handle];
  DBC *cursor;

  for(chandle = 0;
      chandle < MAX_CURSORS && cursors[chandle] != NULL;
      ++chandle);
  if(chandle >= MAX_CURSORS) return -1;

  int foo;
  if((foo = dbp->cursor(dbp, 0, &cursors[chandle], 0))) {
    fprintf(stderr,
            "main(): error `%s'.\n",
            db_strerror(foo));
    return -1;
  } // if
  cursor = cursors[chandle];

  DBT key_dbt, data_dbt;

  int ret;

  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  struct db_key *key;
  key_dbt.size = sizeof(struct db_key);

  struct db_data_int *data;
  data_dbt.size = sizeof(struct db_data_int);
  
  while((ret = cursor->c_get(cursor, &key_dbt, &data_dbt, DB_NEXT)) == 0) {
    key = key_dbt.data;
    data = data_dbt.data;
    fprintf(stderr,
            "[%d %d %d [%d %d]] --> [%d %d]\n",
            key->iid, key->rid, key->tid,
            key->parameters[0], key->parameters[1],
            data->code, data->count);
  } // while

  return 0;

} // main()
