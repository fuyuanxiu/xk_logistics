package com.web.materiel.dao;

import org.springframework.data.jpa.repository.JpaRepository;

import com.web.materiel.entity.MaterielInfo;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface MaterielInfoDao extends CrudRepository<MaterielInfo, Long>, JpaSpecificationExecutor<MaterielInfo> {

    public MaterielInfo findById(long id);

    public List<MaterielInfo> findByIsDel(Integer isDel);

    public List<MaterielInfo> findAllByIsDelOrderByIdAsc(Integer isDel);

    public List<MaterielInfo> findAllByIsDelAndIsBanOrderByIdAsc(Integer isDel, Integer isBan);

    public List<MaterielInfo> findAllByIsDelAndCateNumberFirstIsNotOrderByIdAsc(Integer isDel, String cateNumberFirst);

    //根据物料大类编号获取物料信息
    public List<MaterielInfo> findByIsDelAndCateNumberFirst(Integer isDel, String cateNumberFirst);

    //根据物料类别编号获取物料信息
    public List<MaterielInfo> findByIsDelAndCategoryNumber(Integer isDel, String categoryNumber);

    //根据K3物料编号获取物料信息
    public List<MaterielInfo> findByIsDelAndMateK3Code(Integer isDel, String mateK3Code);

    //根据K3品牌料号获取物料信息
    public List<MaterielInfo> findByIsDelAndMateCusCodeOrderByIdAsc(Integer isDel, String mateCusCode);

    //根据K3品牌和品牌料号获取物料信息
    public List<MaterielInfo> findByIsDelAndMateCusNameAndMateCusCode(Integer isDel, String mateCusName, String mateCusCode);
}
