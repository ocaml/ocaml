/* A DB is a finalized value containing
 *  a pointer to the DB,
 *  a pointer to the openstruct
 *    (this could be removed if we were sure that the library doesn't keep
 *     a pointer to it !)
 */
struct camldb {
  final_fun f;
  DB *db;
  BTREEINFO *info;
  int closed;
};

#define Max_dballoc 1000000

#define Camldb_wosize \
  ((sizeof(struct camldb) + sizeof(value) - 1) / sizeof(value))

#define Camldb_db(v) (((struct camldb *)(Bp_val(v)))->db)
#define Camldb_info(v) (((struct camldb *)(Bp_val(v)))->info)
#define Camldb_closed(v) (((struct camldb *)(Bp_val(v)))->closed)

#define Is_string(v)   (Is_block(v) && (Tag_val(v) == String_tag))
