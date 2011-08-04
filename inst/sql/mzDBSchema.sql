CREATE TABLE experiment(
  _id              INTEGER PRIMARY KEY,
  species          VARCHAR(20) NOT NULL,
  tissue           VARCHAR(50) NOT NULL,
  bio_treatment    VARCHAR(100),
  extraction       VARCHAR(20),
  chromatogram     VARCHAR(20),  
  ms_instrument    VARCHAR(50),
  polarity         VARCHAR(10),
  collision_energy VARCHAR(10),
  data_processing  VARCHAR(100),
  description      VARCHAR(255),
  exp_date         DATE
);

CREATE TABLE mz(
  _id              INTEGER PRIMARY KEY,
  exp_id           INTERGER NOT NULL,
  mz               REAL NOT NULL,
  rt               REAL NOT NULL,  
  ms2              VARCHAR(255),
  ms3              VARCHAR(500),
  annotation       VARCHAR(100),
  comment          VARCHAR(255),
  varification     VARCHAR(100),  
  formula          VARCHAR(100),
  publication      VARCHAR(100),
  publicDB         VARCHAR(100),

  FOREIGN KEY(exp_id) REFERENCES experiment (_id)
);
