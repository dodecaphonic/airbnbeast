import bcrypt from 'bcryptjs';

export const hashPasswordImpl = (password) => {
  return bcrypt.hashSync(password, 10);
};

export const comparePasswordImpl = (password) => (hash) => {
  return bcrypt.compareSync(password, hash);
};